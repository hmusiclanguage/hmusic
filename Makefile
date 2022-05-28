test:
	ghc Tests.hs -e "test"
	javac -cp "./jsyn-20171016.jar:." Track.java JSynBackend/Backend.java
	java -cp "./jsyn-20171016.jar:." Track
