
classpath = ./infjava;antlr.jar

all: grammars compile jar

compile:
	cp WindowsFileInputStream.java ./infjava/WindowsFileInputStream.java
	javac -cp $(classpath) ./infjava/*.java

jar:
	cd infjava
	jar cvfe infjava.jar InfJava *.class
	mv infjava.jar ..
	cd ..

test:
	echo PARSE-FILE	./infjava/InfJava.java	java| java -cp $(classpath) InfJava

grammars:
	ruby build_grammars.rb

antlr:
	wget -nc http://www.antlr.org/download/antlr-4.5-complete.jar -O antlr.jar

clean:
	rm -rf infjava.jar infjava/*
