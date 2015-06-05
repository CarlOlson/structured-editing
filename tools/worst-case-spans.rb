
require 'json'

Span = Struct.new(:name, :from, :to)
depth = ARGV.first.to_i
letters = ('a'..'z').to_a.cycle
out = []

depth.times do |i|
  out << Span.new(letters.next, i + 1, depth).to_a
end

puts out.to_json
