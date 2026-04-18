# ruby-scripts conventions

## Project structure

```
lib/core/          shared utilities — check here first before writing anything new
lib/<domain>/      business logic, one directory per concern (mod_manager, steam, …)
bin/               thin CLI stubs — require, dispatch, rescue only
spec/core/         tests for core utilities
spec/<domain>/     tests for domain code
spec/support/      shared test helpers
```

## Rules

**Before writing any utility code**, check `lib/core/`. If it exists there, use it.

**Any code that could serve more than one domain** belongs in `lib/core/`, not inside a domain directory.

**Bin stubs are thin.** All logic lives in lib. A bin file should require, dispatch to a command function, and have a rescue wrapper at the bottom. No business logic inline.

## Core APIs

| Need | Use |
|------|-----|
| Errors | `Core::Error`, `Core::ValidationError` — raise these from lib; rescue in bin |
| Logging | `Core::Log.debug/info/warn/error` — all output to stdout; level via `RUBY_LOG_LEVEL` |
| XDG paths | `Core::XDG.config_home`, `.data_home`, `.cache_home` |
| File writes | `Core::FileIO.atomic_write(path, content)` — never `File.write` in lib code |
| User prompts | `Core::CLI.confirm(prompt)` — never inline `print`/`$stdin.gets` |

## Error handling pattern

Library code raises, bin rescues:

```ruby
# lib — always raise, never abort/puts errors
raise Core::Error, "something went wrong"
raise Core::ValidationError.new(["field missing: name", "field missing: slug"])

# bin — one rescue block at the bottom
rescue Core::ValidationError => e
  e.errors.each { puts "  #{_1}" }
  exit 1
rescue Core::Error => e
  abort(e.message)
end
```

Never use bare `rescue => e` — unexpected errors are bugs and should propagate.

## Domains

Each domain is a product concern. Add new domains as `lib/<name>/` with specs under `spec/unit/<name>/`.

Current domains:
- `mod_manager` — Cyberpunk 2077 mod lifecycle (deploy, validate, collection editing)
- `steam` — Steam config sync (VDF ↔ TOML)

## Hexagonal architecture (mod_manager)

`lib/mod_manager/` is organized into strict layers:

```
ports/          port interfaces (raise NotImplementedError)
adapters/       I/O implementations of ports (filesystem, nexus API, terminal, memory)
  catalog/      Toml, Memory
  deploy/       LinkFarm, Memory
  download/     Nexus, Memory
  mod_archive/  Filesystem, Memory
  terminal/     Ansi, Memory
services/       pure functions — zero IO; orchestrate domain logic only
interactors/    use-case objects — wire adapters + services; called by CLI
```

**Hard rules:**
- Adapters only know themselves + `lib/core/`. Never call another adapter.
- Services are pure functions. No IO of any kind (`File`, `FileUtils`, `Dir`, etc.).
- Interactors delegate all IO to adapters; call services for domain logic.
- CLI (`cli.rb`) is an inbound adapter: parse args → call interactor → done.

## Test taxonomy

Three tiers — each spec file lives in exactly one:

| Tier | Path | Speed | IO |
|------|------|-------|----|
| Unit | `spec/unit/` | < 1ms | none — plain structs + Memory fakes |
| Integration | `spec/integration/` | < 5ms | Memory fakes only (no tmpfs) |
| System | `spec/system/` | slow | real disk / real APIs |

**Memory fakes** (`Adapters::*::Memory`) are the integration test substrate. Each has `seed(...)` helper(s) and implements the same port as the real adapter. `Adapters::Terminal::Memory` has `stub_confirm(*responses)` and `.output`, `.warnings`, `.errors` helpers.

**Private methods are never tested directly.** They're covered implicitly by tests on public methods. If a private method is complex enough to need its own test, make it public or extract it to a service.
