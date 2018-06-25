# frozen_string_literal: true

require "street_parser/version"
require "street_parser/constants"

module StreetParser
  class US

    class << self
      attr_accessor(
        :street_type_regexp,
        :street_type_matches,
        :number_regexp,
        :fraction_regexp,
        :state_regexp,
        :city_and_state_regexp,
        :direct_regexp,
        :zip_regexp,
        :corner_regexp,
        :street_regexp,
        :po_street_regexp,
        :place_regexp,
        :address_regexp,
        :po_address_regexp,
        :informal_address_regexp,
        :dircode_regexp,
        :unit_prefix_numbered_regexp,
        :unit_prefix_unnumbered_regexp,
        :unit_regexp,
        :sep_regexp,
        :sep_avoid_unit_regexp,
        :intersection_regexp
      )
    end

    self.street_type_matches = {}
    STREET_TYPES.each_pair { |type,abbrv|
      self.street_type_matches[abbrv] = /\b (?: #{abbrv}|#{Regexp.quote(type)} ) \b/ix
    }

    self.street_type_regexp = Regexp.new(STREET_TYPES_LIST.keys.join("|"), Regexp::IGNORECASE)
    self.fraction_regexp = /\d+\/\d+/
    self.state_regexp = Regexp.new(
      '\b' + STATE_CODES.flatten.map{ |code| Regexp.quote(code) }.join("|") + '\b',
      Regexp::IGNORECASE
    )
    self.direct_regexp = Regexp.new(
      (DIRECTIONAL.keys +
       DIRECTIONAL.values.sort { |a,b|
         b.length <=> a.length
       }.map { |c|
         f = c.gsub(/(\w)/, '\1.')
         [Regexp::quote(f), Regexp::quote(c)]
       }
      ).join("|"),
      Regexp::IGNORECASE
    )
    self.dircode_regexp = Regexp.new(DIRECTION_CODES.keys.join("|"), Regexp::IGNORECASE)
    self.zip_regexp     = /(?:(?<postal_code>\d{5})(?:-?(?<postal_code_ext>\d{4}))?)/
    self.corner_regexp  = /(?:\band\b|\bat\b|&|\@)/i

    # we don't include letters in the number regex because we want to
    # treat "42S" as "42 S" (42 South). For example,
    # Utah and Wisconsin have a more elaborate system of block numbering
    # http://en.wikipedia.org/wiki/House_number#Block_numbers
    self.number_regexp = /(?<number>\d+-?\d*)(?=\D)/ix

    # note that expressions like [^,]+ may scan more than you expect
    self.street_regexp = /
      (?:
        # special case for addresses like 100 South Street
        (?:(?<street> #{direct_regexp})\W+
           (?<street_type> #{street_type_regexp})\b
        )
        |
        (?:(?<prefix> #{direct_regexp})\W+)?
        (?:
          (?<street> [^,]*\d)
          (?:[^\w,]* (?<suffix> #{direct_regexp})\b)
          |
          (?<street> [^,]+)
          (?:[^\w,]+(?<street_type> #{street_type_regexp})\b)
          (?:[^\w,]+(?<suffix> #{direct_regexp})\b)?
          |
          (?<street> [^,]+?)
          (?:[^\w,]+(?<street_type> #{street_type_regexp})\b)?
          (?:[^\w,]+(?<suffix> #{direct_regexp})\b)?
        )
      )
    /ix;

    self.po_street_regexp = /^(?<street>p\.?o\.?\s?(?:box|\#)?\s\d\d*[-a-z]*)/ix;

    # http://pe.usps.com/text/pub28/pub28c2_003.htm
    # TODO add support for those that don't require a number
    # TODO map to standard names/abbreviations
    self.unit_prefix_numbered_regexp = /
      (?<unit_prefix>
        su?i?te
        |p\W*[om]\W*b(?:ox)?
        |(?:ap|dep)(?:ar)?t(?:me?nt)?
        |ro*m
        |flo*r?
        |uni?t
        |bu?i?ldi?n?g
        |ha?nga?r
        |lo?t
        |pier
        |slip
        |spa?ce?
        |stop
        |tra?i?le?r
        |box)(?![a-z])
    /ix;

    self.unit_prefix_unnumbered_regexp = /
      (?<unit_prefix>
        ba?se?me?n?t
        |fro?nt
        |lo?bby
        |lowe?r
        |off?i?ce?
        |pe?n?t?ho?u?s?e?
        |rear
        |side
        |uppe?r
        )\b
    /ix;

    self.unit_regexp = /
      (?:
          (?: (?:#{unit_prefix_numbered_regexp} \W*)
              | (?<unit_prefix> \#)\W*
          )
          (?<unit> [\w-]+)
      )
      |
      #{unit_prefix_unnumbered_regexp}
    /ix;

    self.city_and_state_regexp = /
      (?:
          (?<city> [^\d,]+?)\W+
          (?<state> #{state_regexp})
      )
    /ix;

    self.place_regexp = /
      (?:#{city_and_state_regexp}\W*)? (?:#{zip_regexp})?
    /ix;

    self.address_regexp = /
      \A
      [^\w\x23]*    # skip non-word chars except # (eg unit)
      #{number_regexp} \W*
      (?:#{fraction_regexp}\W*)?
      #{street_regexp}\W+
      (?:#{unit_regexp}\W+)?
      #{place_regexp}
      \W*         # require on non-word chars at end
      \z           # right up to end of string
    /ix;

    self.po_address_regexp = /
      \A
      #{po_street_regexp} \W*
      #{place_regexp}
      \W*         # require on non-word chars at end
      \z           # right up to end of string
    /ix;

    self.sep_regexp = /(?:\W+|\Z)/;
    self.sep_avoid_unit_regexp = /(?:[^\#\w]+|\Z)/;

    self.informal_address_regexp = /
      \A
      \s*         # skip leading whitespace
      (?:#{unit_regexp} #{sep_regexp})?
      (?:#{number_regexp})? \W*
      (?:#{fraction_regexp} \W*)?
      #{street_regexp} #{sep_avoid_unit_regexp}
      (?:#{unit_regexp} #{sep_regexp})?
      (?:#{place_regexp})?
      # don't require match to reach end of string
    /ix;

    self.intersection_regexp = /\A\W*
      #{street_regexp}\W*?

      \s+#{corner_regexp}\s+

#          (?{ exists $_{$_} && $_{$_.1} = delete $_{$_} for (qw{prefix street type suffix})})
      #{street_regexp}\W+
#          (?{ exists $_{$_} && $_{$_.2} = delete $_{$_} for (qw{prefix street type suffix})})

      #{place_regexp}
      \W*\z
    /ix;

    class << self
      def parse(location, args={})
        if( corner_regexp.match(location) )
          return parse_intersection(location, args)
        else
          return parse_po_address(location, args) || parse_address(location, args) || parse_informal_address(location, args)
        end
      end

      def parse_address(address, args={})
        return unless match = address_regexp.match(address)

        to_address( match_to_hash(match), args )
      end

      def parse_po_address(address, args={})
        return unless match = po_address_regexp.match(address)

        to_address(match_to_hash(match), args)
      end

      def parse_informal_address(address, args={})
        return unless match = informal_address_regexp.match(address)

        to_address( match_to_hash(match), args )
      end

      def parse_intersection(intersection, args)
        return unless match = intersection_regexp.match(intersection)

        hash = match_to_hash(match)

        streets = intersection_regexp.named_captures["street"].map { |pos|
          match[pos.to_i]
        }.select { |v| v }
        hash["street"]  = streets[0] if streets[0]
        hash["street2"] = streets[1] if streets[1]

        street_types = intersection_regexp.named_captures["street_type"].map { |pos|
          match[pos.to_i]
        }.select { |v| v }
        hash["street_type"]  = street_types[0] if street_types[0]
        hash["street_type2"] = street_types[1] if street_types[1]

        if(
          hash["street_type"] &&
          (
            !hash["street_type2"] ||
            (hash["street_type"] == hash["street_type2"])
          )
        )
          type = hash["street_type"].clone
          if( type.gsub!(/s\W*$/i, '') && /\A#{street_type_regexp}\z/i =~ type )
            hash["street_type"] = hash["street_type2"] = type
          end
        end

        to_address( hash, args )
      end

      private
        def match_to_hash(match)
          hash = {}
          match.names.each { |name| hash[name] = match[name] if match[name] }
          return hash
        end

        def to_address(input, args)
          # strip off some punctuation and whitespace
          input.values.each { |string|
            string.strip!
            string.gsub!(/[^\w\s\-\#\&]/, '')
          }

          input['redundant_street_type'] = false
          if( input['street'] && !input['street_type'] )
            match = street_regexp.match(input['street'])
            input['street_type'] = match['street_type']
          input['redundant_street_type'] = true
          end

          NORMALIZE_MAP.each_pair { |key, map|
            next unless input[key]
            mapping = map[input[key].downcase]
            input[key] = mapping if mapping
          }

          if( args[:avoid_redundant_street_type] )
            ['', '1', '2'].each { |suffix|
              street = input['street'      + suffix];
              type   = input['street_type' + suffix];
              next if !street || !type

              type_regexp = street_type_matches[type.downcase] # || fail "No STREET_TYPE_MATCH for #{type}"
              input.delete('street_type' + suffix) if type_regexp.match(street)
            }
          end

          # attempt to expand directional prefixes on place names
          if( input['city'] )
            input['city'].gsub!(/^(#{dircode_regexp})\s+(?=\S)/) { |m|
              DIRECTION_CODES[m[0].upcase] + ' '
            }
          end

          %w(street street_type street2 street_type2 city unit_prefix).each do |k|
            input[k] = input[k].split.map(&:capitalize).join(' ') if input[k]
          end

          return StreetParser::US::Address.new( input )
        end
    end

    class Address
      attr_reader :state
      attr_accessor(
        :number,
        :street,
        :street_type,
        :unit,
        :unit_prefix,
        :suffix,
        :prefix,
        :city,
        :postal_code,
        :postal_code_ext,
        :street2,
        :street_type2,
        :suffix2,
        :prefix2,
        :redundant_street_type
      )

      def initialize(args)
        args.each do |attr, val|
          public_send("#{attr}=", val)
        end
      end

      def state=(value)
        value = value.upcase if value.size <= 2
        @state = value
      end

      def full_postal_code
        return nil unless self.postal_code
        self.postal_code_ext ? "#{postal_code}-#{postal_code_ext}" : self.postal_code
      end


      def state_fips
        StreetParser::US::FIPS_STATES[state]
      end

      def state_name
        name = StreetParser::US::STATE_NAMES[state] && name.capitalize
      end

      def intersection?
        !street2.nil?
      end

      def line1(s = "")
        parts = []
        if intersection?
          parts << prefix       if prefix
          parts << street
          parts << street_type  if street_type
          parts << suffix       if suffix
          parts << 'and'
          parts << prefix2      if prefix2
          parts << street2
          parts << street_type2 if street_type2
          parts << suffix2      if suffix2
        else
          parts << number
          parts << prefix if prefix
          parts << street if street
          parts << street_type if street_type && !redundant_street_type
          parts << suffix if suffix
          parts << unit_prefix if unit_prefix
          #follow guidelines: http://pe.usps.gov/cpim/ftp/pubs/Pub28/pub28.pdf pg28
          parts << (unit_prefix ? unit : "\# #{unit}") if unit
        end
        s + parts.join(' ').strip
      end


      def line2(s = "")
        parts = []
        parts << city  if city
        parts << state if state
        s = s + parts.join(', ')
        if postal_code
          s << " #{postal_code}"
          s << "-#{postal_code_ext}" if postal_code_ext
        end
        s.strip
      end


      def to_s(format = :default)
        s = ""
        case format
        when :line1
          s << line1(s)
        when :line2
          s << line2(s)
        when :street_address_1
          s << street_address_1
        when :street_address_2
          s << street_address_2
        when :city_state_zip
          s << line2(s)
        else
          s << [line1, line2].select{ |l| !l.empty? }.join(', ')
        end
        s
      end

      def street_address_1
        s = ""
        s = number + " " unless number.nil?
        s += prefix + " " unless prefix.nil?
        s += street + " " unless street.nil?
        s += street_type unless street_type.nil?
        s.strip
      end

      def street_address_2
        s = ""
        if( !unit_prefix.nil? && !unit.nil? )
          s += unit_prefix + " " + unit
        elsif( unit_prefix.nil? && !unit.nil? )
          s += "#" + unit
        end
        s
      end

      def to_h
        self.instance_variables.each_with_object({}) do |var_name, hash|
          var_value = self.instance_variable_get(var_name)
          hash_name = var_name[1..-1].to_sym
          hash[hash_name] = var_value
        end
      end

      def ==(other)
        to_s == other.to_s
      end
    end
  end
end
