# frozen_string_literal: true

require "minitest/autorun"
require "tmpdir"
require "nexus/fomod"

FIXTURE_XML = File.join(__dir__, "../fixtures/nexus/fomod_moduleconfig.xml")

class FomodDetectTest < Minitest::Test
  def test_returns_nil_when_no_xml
    Dir.mktmpdir do |dir|
      assert_nil Nexus::Fomod.detect(dir)
    end
  end

  def test_returns_choices_when_xml_present
    Dir.mktmpdir do |dir|
      FileUtils.mkdir_p(File.join(dir, "fomod"))
      FileUtils.cp(FIXTURE_XML, File.join(dir, "fomod", "ModuleConfig.xml"))
      choices = Nexus::Fomod.detect(dir)
      refute_nil choices
      assert_equal 2, choices.size
    end
  end
end

class FomodParseTest < Minitest::Test
  def setup
    @choices = Nexus::Fomod.parse(FIXTURE_XML)
  end

  def test_returns_two_choices
    assert_equal 2, @choices.size
  end

  def test_first_choice_name
    assert_equal "Welcome to Night City", @choices[0].name
  end

  def test_second_choice_name
    assert_equal "Cyberpunk THING", @choices[1].name
  end

  def test_choices_have_descriptions
    assert @choices[0].description.length > 0
    assert @choices[1].description.length > 0
  end

  def test_folders_map_source_to_destination
    assert_equal "Welcome to Night City", @choices[0].folders.first["source"]
    assert_equal "",                      @choices[0].folders.first["destination"]
    assert_equal "Cyberpunk THING",       @choices[1].folders.first["source"]
  end

  def test_multiple_groups_raises
    Dir.mktmpdir do |dir|
      xml = <<~XML
        <?xml version="1.0" encoding="UTF-8"?>
        <config>
          <installSteps>
            <installStep name="Step1">
              <optionalFileGroups>
                <group name="A" type="SelectExactlyOne"><plugins/></group>
                <group name="B" type="SelectExactlyOne"><plugins/></group>
              </optionalFileGroups>
            </installStep>
          </installSteps>
        </config>
      XML
      path = File.join(dir, "ModuleConfig.xml")
      File.write(path, xml)
      assert_raises(Core::Error) { Nexus::Fomod.parse(path) }
    end
  end

  def test_no_select_exactly_one_returns_nil
    Dir.mktmpdir do |dir|
      xml = <<~XML
        <?xml version="1.0" encoding="UTF-8"?>
        <config>
          <installSteps>
            <installStep name="Step1">
              <optionalFileGroups>
                <group name="A" type="SelectAny"><plugins/></group>
              </optionalFileGroups>
            </installStep>
          </installSteps>
        </config>
      XML
      path = File.join(dir, "ModuleConfig.xml")
      File.write(path, xml)
      assert_nil Nexus::Fomod.parse(path)
    end
  end
end
