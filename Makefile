test:
	ghc JSynCompiler.hs -e "test"
	javac -cp "./jsyn-20171016.jar:." Track.java JSynBackend/Backend.java -Xlint:deprecation
	java -cp "./jsyn-20171016.jar:." Track
