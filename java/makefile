
classpath = ./infjava;./infjava/grammars;antlr.jar

all: antlr grammars compile

compile:
	javac -cp $(classpath) ./infjava/*.java

jar:
	cd infjava
	jar cvfe infjava.jar InfJava *.class grammars/*.class
	mv infjava.jar ..
	cd ..

test:
	echo PARSE-FILE	./infjava/InfJava.java	java| java -cp $(classpath) InfJava

grammars:
	ruby build_grammars.rb
	javac -cp $(classpath) ./infjava/grammars/*.java

antlr:
	wget -nc http://www.antlr.org/download/antlr-4.5-complete.jar -O antlr.jar

clean:
	rm -rf antlr.jar infjava/grammars infjava/*.class infjava/InfJava.java
