
# Not all grammars will build.

# Lexers must be built before Parsers; however, Dir#glob should return
# alphabetic order

require 'pathname'
require 'csv'
require 'erb'

OUT_DIR = './infjava/grammars'

grammars = []

CSV.parse(DATA.read).each do |(filename, start_rule)|
  pn = Pathname.new filename
  name = pn.basename.sub_ext("").to_s
  grammars << [name, start_rule]
  
  if not pn.exist?
    puts "#{filename.to_s} is missing."
    next
  end
  
  if File.exist? "antlr.jar"
    system "java", "-cp", "antlr.jar", "org.antlr.v4.Tool", "-visitor", filename, "-o", OUT_DIR
  else
    system "antlr4", "-visitor", filename, "-o", OUT_DIR
  end
end

template = File.open("./InfJava.java.erb", "r") { |f| f.read }
source = ERB.new(template).result
File.open("./InfJava/InfJava.java","w") { |f| f.write source }

__END__
"./grammars-v4/java/Java.g4","compilationUnit"
"./grammars-v4/java8/Java8.g4","compilationUnit"
