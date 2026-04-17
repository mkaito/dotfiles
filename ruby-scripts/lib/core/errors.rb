# frozen_string_literal: true

module Core
  Error = Class.new(StandardError)

  ValidationError = Class.new(Error) do
    attr_reader :errors

    def initialize(errors)
      @errors = errors
      super(errors.join("; "))
    end
  end
end
