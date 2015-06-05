
require "json"
require_relative "ruby_processor.rb"

if ARGV.first.nil? or
    not File.exist? ARGV.first
  exit 1
end

source = File.open(ARGV.first, "r") { |f| f.read }
p = RubyProcessor.parse source

puts p.spans.to_json
