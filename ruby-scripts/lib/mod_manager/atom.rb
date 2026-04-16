# frozen_string_literal: true

module ModManager
  module Atom
    OPERATORS = { ">=" => :gte, "<=" => :lte, ">" => :gt, "<" => :lt, "=" => :eq }.freeze

    def self.parse(str)
      op = :latest

      OPERATORS.each do |prefix, sym|
        next unless str.start_with?(prefix)
        op  = sym
        str = str[prefix.length..]
        break
      end

      return { slug: str, op: :latest, version: nil } if op == :latest

      m = str.match(/\A(.+)-(\d[\w.]*)\z/) or raise Error, "invalid atom: #{str.inspect}"
      { slug: m[1], op:, version: m[2] }
    end

    def self.resolve(atom_str, archive)
      a = parse(atom_str)
      case a[:op]
      when :latest then archive.latest(a[:slug])
      when :eq     then archive.get(a[:slug], a[:version])
      else
        archive.versions(a[:slug]).find { satisfies?(_1.version, a[:op], a[:version]) }
      end
    end

    private_class_method def self.satisfies?(ver, op, constraint)
      cmp = compare(ver, constraint)
      case op
      when :gte then cmp >= 0
      when :gt  then cmp > 0
      when :lte then cmp <= 0
      when :lt  then cmp < 0
      end
    end

    private_class_method def self.compare(v1, v2)
      parts = ->(v) { v.to_s.split(".").map(&:to_i) }
      a, b  = parts.(v1), parts.(v2)
      len   = [a.size, b.size].max
      a.fill(0, a.size...len)
      b.fill(0, b.size...len)
      a <=> b
    end
  end
end
