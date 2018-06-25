# frozen_string_literal: true

lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "street_parser/version"

Gem::Specification.new do |spec|
  spec.name          = "street_parser"
  spec.version       = StreetParser::VERSION
  spec.authors       = ["Ben Sharpe"]
  spec.email         = ["bsharpe@gmail.com"]

  spec.summary       = %q{Parse Addresses into substituent parts. This gem includes US only.}
  spec.description   = %q{StreetAddress::US allows you to send any string to parse and if the string is a US address returns an object of the address broken into it's substituent parts.
A port of Geo::StreetAddress::US by Schuyler D. Erle and Tim Bunce.}
  spec.homepage      = "https://github.com/bsharpe/street_parser"
  spec.license       = "MIT"

  # Prevent pushing this gem to RubyGems.org. To allow pushes either set the 'allowed_push_host'
  # to allow pushing to a single host or delete this section to allow pushing to any host.
  # if spec.respond_to?(:metadata)
  #   spec.metadata["allowed_push_host"] = "TODO: Set to 'http://mygemserver.com'"
  # else
  #   raise "RubyGems 2.0 or newer is required to protect against " \
  #     "public gem pushes."
  # end

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files         = Dir.chdir(File.expand_path('..', __FILE__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.16"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec", "~> 3.0"
end