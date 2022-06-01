test:
	ghc Tests.hs -e "test"
	javac -cp "./jsyn-20171016.jar:." Track.java JavaRuntime/Runtime.java
	java -cp "./jsyn-20171016.jar:." Track
