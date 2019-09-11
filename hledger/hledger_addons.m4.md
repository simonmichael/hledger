# ADD-ON COMMANDS

hledger also searches for external add-on commands, and will include these in the commands list.
These are programs or scripts in your PATH whose name starts with `hledger-`
and ends with a recognised file extension 
(currently: no extension, `bat`,`com`,`exe`, `hs`,`lhs`,`pl`,`py`,`rb`,`rkt`,`sh`).

Add-ons can be invoked like any hledger command, but there are a few things to be aware of.
Eg if the `hledger-web` add-on is installed,

- `hledger -h web` shows hledger's help, while `hledger web -h` shows hledger-web's help.
  
- Flags specific to the add-on must have a preceding `--` to hide them from hledger.
  So `hledger web --serve --port 9000` will be rejected; you must use `hledger web -- --serve --port 9000`.

- You can always run add-ons directly if preferred: `hledger-web --serve --port 9000`.

Add-ons are a relatively easy way to add local features or experiment with new ideas.
They can be written in any language, but haskell scripts have a big advantage:
they can use the same hledger (and haskell) library functions that built-in commands do,
for command-line options, journal parsing, reporting, etc.

Here are some hledger add-ons available:

## Official add-ons

These are maintained and released along with hledger.   

### ui
[hledger-ui](hledger-ui.html) provides an efficient terminal interface. 

### web
[hledger-web](hledger-web.html) provides a simple web interface.

## Third party add-ons

These are maintained separately, and usually updated shortly after a hledger release.

### diff

[hledger-diff](http://hackage.haskell.org/package/hledger-diff)
shows differences in an account's transactions between one journal file and another.

### iadd

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd)
is a more interactive, terminal UI replacement for the [add command](hledger.html#add). 

### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
generates interest transactions for an account according to various schemes. 

### irr
[hledger-irr](http://hackage.haskell.org/package/hledger-irr)
calculates the internal rate of return of an investment account,
but it's superseded now by the built-in [roi](#roi) command. 

## Experimental add-ons
  
These are available in source form in the hledger repo's bin/ directory.
They may be less mature and documented than built-in commands.
Reading and tweaking these is a good way to start making your own!

### autosync

[hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync) 
is a symbolic link for easily running 
[ledger-autosync](https://pypi.python.org/pypi/ledger-autosync), if installed. 
ledger-autosync does deduplicating conversion of OFX data and some CSV formats,
and can also download the data 
[if your bank offers OFX Direct Connect](http://wiki.gnucash.org/wiki/OFX_Direct_Connect_Bank_Settings). 

### chart

[hledger-chart.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-chart.hs#L47)
is an old pie chart generator, in need of some love.

### check

[hledger-check.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check.hs)
checks more powerful account balance assertions.

