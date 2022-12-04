# Make

A Makefile is provided to make common developer tasks easy to remember,
and to insulate us a little from the ever-evolving Haskell tools ecosystem.
Using it is entirely optional, but recommended.
You'll need [GNU Make](https://www.gnu.org/software/make) installed.

The Makefile contains a fair amount of obsolete cruft and needs cleanup. Some tasks (docs, website) are now handled by the [Shake](#shake) file instead.

The Makefile is self-documenting. Run `make` to see a list of the main make rules:

```shell
$ make
Makefile:37: -------------------- hledger make rules --------------------
Makefile:39: make [help] -- list documented rules in this makefile. make -n RULE shows more detail.
Makefile:204: (INSTALLING:)
Makefile:206: make install -- download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack)
Makefile:231: (BUILDING:)
Makefile:235: make build -- download dependencies and build hledger executables (with stack)
Makefile:304: make hledgerdev -- quickly build the hledger executable (with ghc and -DDEVELOPMENT)
...
```

To see what a make rule will do without actually doing it, use the `-n` flag:

```shell
$ make build -n
stack build
```
```shell
$ make test -n
(stack test \
		&& echo pkgtest PASSED) || echo pkgtest FAILED
(stack exec hledger test \
		&& echo builtintest PASSED) || echo builtintest FAILED
(COLUMNS=80 PATH=`pwd`/bin:/home/simon/src/hledger/bin:/home/simon/.local/bin:/home/simon/.cabal/bin:/opt/ghc/7.10.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/var/lib/gems/1.9.1/bin stack exec -- shelltest --execdir -- -j16 --hide-successes tests \
		&& echo functest PASSED) || echo functest FAILED
```


