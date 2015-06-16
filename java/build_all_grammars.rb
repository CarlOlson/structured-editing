
# Not all grammars will build.

# Lexers must be built before Parsers; however, Dir#glob should return
# alphabetic order

require 'pathname'

export_dir = Pathname.new 'built_grammars'

Dir.glob("grammars-v4/**/*.g4").each do |filename|
  next if filename =~ /aspectj/ # aspectj requires extra work
  dir = export_dir + Pathname.new(filename).dirname.basename
  system "antlr4", filename, "-o", dir.to_s
end
