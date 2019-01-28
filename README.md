# hmusic
A DSL for music programming and live coding

* You need to install Sonic Pi

https://sonic-pi.net/


* You need to install Sonic Pi tool

https://github.com/lpil/sonic-pi-tool

* You need to change in the source code the variable sonicPiToolPath to point to your local
installation of the Sonic Pi tool, e.g.:

sonicPiToolPath :: String

sonicPiToolPath = "/home/john/.cargo/bin/"

* Start sonic Pi before using HMusic

* Load the script in Ghci:

Prelude> :l HMusic.hs

* test

*HMusic> loop 120 t1

* OBS: HMusic was only tested on linux


