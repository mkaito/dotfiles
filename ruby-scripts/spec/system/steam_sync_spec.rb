# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "fileutils"
require "json"

# End-to-end: spawn the actual bin against a throwaway HOME/XDG, with a fake
# `pgrep` on PATH so the "Steam is running" guard is deterministic (never
# matches), independent of whether real Steam is running on the dev machine.
class SteamSyncSystemTest < Minitest::Test
  ROOT = File.expand_path("../..", __dir__)
  BIN = File.join(ROOT, "bin", "steam-launch-sync")
  LIB = File.join(ROOT, "lib")

  SAMPLE = begin
    t = ->(n) { "\t" * n }
    [
      %("UserLocalConfigStore"), %({),
      %(#{t[1]}"Software"), %(#{t[1]}{),
      %(#{t[2]}"Valve"), %(#{t[2]}{),
      %(#{t[3]}"Steam"), %(#{t[3]}{),
      %(#{t[4]}"apps"), %(#{t[4]}{),
      %(#{t[5]}"100"), %(#{t[5]}{),
      %(#{t[6]}"LaunchOptions"\t\t"mangohud %command%"),
      %(#{t[6]}"LastPlayed"\t\t"123"),
      %(#{t[5]}}),
      %(#{t[5]}"200"), %(#{t[5]}{),
      %(#{t[6]}"LastPlayed"\t\t"456"),
      %(#{t[5]}}),
      %(#{t[4]}}), %(#{t[3]}}), %(#{t[2]}}), %(#{t[1]}}), %(}), ""
    ].join("\n")
  end

  def with_sandbox
    Dir.mktmpdir do |home|
      vdf = File.join(home, ".local/share/Steam/userdata/123/config/localconfig.vdf")
      FileUtils.mkdir_p(File.dirname(vdf))
      File.write(vdf, SAMPLE)

      bindir = File.join(home, "fakebin")
      FileUtils.mkdir_p(bindir)
      pgrep = File.join(bindir, "pgrep")
      File.write(pgrep, "#!/bin/sh\nexit 1\n") # never "finds" steam
      File.chmod(0o755, pgrep)

      env = {
        "HOME" => home,
        "PATH" => "#{bindir}:#{ENV["PATH"]}"
      }
      backup = File.join(home, "launch-options.json")
      yield(env, vdf, backup)
    end
  end

  def run_sync(env, *args)
    ok = system(env, "ruby", "-I#{LIB}", BIN, *args, out: File::NULL)
    assert ok, "steam-launch-sync #{args.join(" ")} exited non-zero"
  end

  def test_import_writes_backup_json
    with_sandbox do |env, _vdf, backup|
      run_sync(env, "import", backup)
      assert_equal({"100" => "mangohud %command%"}, JSON.parse(File.read(backup)))
    end
  end

  def test_export_is_surgical_and_round_trips
    with_sandbox do |env, vdf, backup|
      original = File.read(vdf)
      File.write(backup, JSON.generate({"100" => "newopts", "200" => "added", "999" => "skip"}))

      run_sync(env, "export", backup)
      result = File.read(vdf)

      # only the intended bytes changed: replace 100's value, insert into 200
      expected = original
        .sub('"mangohud %command%"', '"newopts"')
        .sub(%(\t\t\t\t\t"200"\n\t\t\t\t\t{\n), %(\t\t\t\t\t"200"\n\t\t\t\t\t{\n\t\t\t\t\t\t"LaunchOptions"\t\t"added"\n))
      assert_equal expected, result
    end
  end
end
