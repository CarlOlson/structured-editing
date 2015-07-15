
classpath = .;antlr.jar;grammars;infjava

all: antlr grammars compile

compile:
	javac -cp $(classpath) ./infjava/*.java

test:
	echo PARSE-FILE	./infjava/InfJava.java	java| java -cp $(classpath) InfJava

grammars:
	ruby build_grammars.rb
	javac -cp $(classpath) ./grammars/*.java

antlr:
	wget -nc http://www.antlr.org/download/antlr-4.5-complete.jar -O antlr.jar

clean:
	rm -rf antlr.jar grammars infjava/*.class infjava/InfJava.java
