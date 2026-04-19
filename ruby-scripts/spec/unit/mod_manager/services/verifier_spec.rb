# frozen_string_literal: true

require "minitest/autorun"
require "mod_manager/services/verifier"

include ModManager

class VerifierTest < Minitest::Test
  Check = Services::Verifier::Check

  def test_no_errors_when_all_present
    checks = [
      Check.new(path: "bin/x64", type: "dir", present: true),
      Check.new(path: "bin/x64/global.ini", type: "file", present: true)
    ]
    assert_empty Services::Verifier.run(checks)
  end

  def test_missing_dir_reported
    checks = [Check.new(path: "bin/x64", type: "dir", present: false)]
    errors = Services::Verifier.run(checks)
    assert errors.any? { it.include?("bin/x64") }
  end

  def test_missing_file_reported
    checks = [Check.new(path: "bin/x64/global.ini", type: "file", present: false)]
    errors = Services::Verifier.run(checks)
    assert errors.any? { it.include?("bin/x64/global.ini") }
  end

  def test_unknown_type_reported
    checks = [Check.new(path: "some/path", type: "symlink", present: false)]
    errors = Services::Verifier.run(checks)
    assert errors.any? { it.include?("unknown check type") }
  end

  def test_only_absent_checks_produce_errors
    checks = [
      Check.new(path: "present-dir", type: "dir", present: true),
      Check.new(path: "absent-dir", type: "dir", present: false)
    ]
    errors = Services::Verifier.run(checks)
    assert_equal 1, errors.size
    assert errors.first.include?("absent-dir")
  end
end
