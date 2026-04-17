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

Each domain is a product concern. Add new domains as `lib/<name>/` with specs under `spec/<name>/`.

Current domains:
- `mod_manager` — Cyberpunk 2077 mod lifecycle (deploy, validate, collection editing)
- `steam` — Steam config sync (VDF ↔ TOML)
