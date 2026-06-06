# frozen_string_literal: true

require "core/errors"

# Minimal, dependency-free reader/surgical-editor for Steam's text VDF
# (KeyValues) format, scoped to what launch-option backup needs.
#
# Values are treated as OPAQUE: the raw bytes between the delimiting quotes are
# captured and re-emitted verbatim (backslashes and escaped quotes included). A
# value read from a valid VDF always re-emits to a valid VDF, so no escape
# interpretation is needed. The only parsing rule is finding the closing quote:
# a `"` preceded by an even number of backslashes.
module Steam
  module Vdf
    APPS_SCOPE = %w[userlocalconfigstore software valve steam apps].freeze
    LAUNCH_KEY = "launchoptions"

    Token = Struct.new(:type, :raw, :tstart, :tend, :vstart, :vend)

    module_function

    # => { appid => raw_launch_options } for non-empty values under apps.
    def launch_options(src)
      out = {}
      walk(tokenize(src)) do |path, key_tok, val_tok|
        next unless in_app_scope?(path) && key_tok.raw.casecmp?(LAUNCH_KEY)
        next if val_tok.raw.empty?
        out[path[-2]] = val_tok.raw
      end
      out
    end

    # => array of every appid block under apps (regardless of launch options).
    def app_ids(src)
      scan_apps(src).keys
    end

    # Surgically set launch options for each appid present in the VDF.
    # Replaces the value bytes in place, or inserts one line if absent. Every
    # other byte of `src` is preserved. appids not present are reported skipped.
    # => { src:, applied:, skipped: }
    def apply_launch_options(src, map)
      blocks = scan_apps(src)
      edits = []
      applied = 0
      skipped = []

      map.each do |appid, value|
        info = blocks[appid]
        unless info
          skipped << appid
          next
        end
        edits << if info[:lo_vstart]
          [info[:lo_vstart], info[:lo_vend], value.to_s]
        else
          lead = "\n#{info[:child_indent]}"
          [info[:open_end], info[:open_end], %(#{lead}"LaunchOptions"\t\t"#{value}")]
        end
        applied += 1
      end

      result = src.dup
      edits.sort_by { |start, _, _| -start }.each do |start, stop, text|
        result[start...stop] = text
      end

      {src: result, applied: applied, skipped: skipped}
    end

    # ── internals ──────────────────────────────────────────────────────────

    # Lex into quoted-string / `{` / `}` tokens with byte offsets.
    def tokenize(src)
      tokens = []
      i = 0
      len = src.length
      while i < len
        c = src[i]
        case c
        when /\s/
          i += 1
        when "{"
          tokens << Token.new(:open, nil, i, i + 1)
          i += 1
        when "}"
          tokens << Token.new(:close, nil, i, i + 1)
          i += 1
        when '"'
          vstart = i + 1
          j = vstart
          j += 1 until j >= len && raise_unterminated || (src[j] == '"' && even_backslashes?(src, j))
          tokens << Token.new(:str, src[vstart...j], i, j + 1, vstart, j)
          i = j + 1
        else
          raise Core::Error, "VDF parse error: unexpected #{c.inspect} at offset #{i}"
        end
      end
      tokens
    end

    def raise_unterminated
      raise Core::Error, "VDF parse error: unterminated string"
    end

    # True when the `"` at index j is a real delimiter (even backslash run before it).
    def even_backslashes?(src, j)
      bs = 0
      k = j - 1
      while k >= 0 && src[k] == "\\"
        bs += 1
        k -= 1
      end
      bs.even?
    end

    # Walk tokens as KeyValues; yield (path, key_token, value_token) for each leaf.
    # `path` is the array of enclosing block keys plus the leaf key.
    def walk(tokens)
      path = []
      pending = nil
      tokens.each do |tok|
        case tok.type
        when :str
          if pending
            yield(path + [pending.raw], pending, tok)
            pending = nil
          else
            pending = tok
          end
        when :open
          raise Core::Error, "VDF parse error: '{' without key" unless pending
          path.push(pending.raw)
          pending = nil
        when :close
          path.pop
          pending = nil
        end
      end
      path
    end

    # => { appid => {open_end:, child_indent:, lo_vstart:, lo_vend:} } for each
    # appid block under apps. lo_* are nil when the block has no LaunchOptions.
    def scan_apps(src)
      tokens = tokenize(src)
      blocks = {}
      path = []
      pending = nil

      tokens.each do |tok|
        case tok.type
        when :str
          if pending
            # leaf: pending=key, tok=value
            if current_app(path) && pending.raw.casecmp?(LAUNCH_KEY)
              b = blocks[path.last]
              b[:lo_vstart] = tok.vstart
              b[:lo_vend] = tok.vend
            end
            pending = nil
          else
            pending = tok
          end
        when :open
          raise Core::Error, "VDF parse error: '{' without key" unless pending
          path.push(pending.raw)
          if current_app(path)
            indent = line_indent(src, pending.tstart)
            blocks[pending.raw] = {open_end: tok.tend, child_indent: "#{indent}\t", lo_vstart: nil, lo_vend: nil}
          end
          pending = nil
        when :close
          path.pop
          pending = nil
        end
      end

      blocks
    end

    # path is exactly inside an appid block: [..apps, appid]
    def current_app(path)
      path.length == APPS_SCOPE.length + 1 && in_apps?(path[0...APPS_SCOPE.length])
    end

    # path is exactly an appid leaf scope for launch_options' yielded path (incl leaf key removed)
    def in_app_scope?(path)
      # path here includes the leaf key as last element; the block path is path[0..-2]
      path.length == APPS_SCOPE.length + 2 && in_apps?(path[0...APPS_SCOPE.length])
    end

    def in_apps?(prefix)
      prefix.map(&:downcase) == APPS_SCOPE
    end

    # The whitespace between the preceding newline and offset `off`.
    def line_indent(src, off)
      nl = src.rindex("\n", off - 1)
      start = nl ? nl + 1 : 0
      src[start...off]
    end
  end
end
