# frozen_string_literal: true

module Core
  module FileIO
    def self.atomic_write(path, content)
      tmp = "#{path}.tmp.#{Process.pid}"
      File.write(tmp, content)
      File.rename(tmp, path)
    end
  end
end
