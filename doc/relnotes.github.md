<details>
<summary>

## Release notes (https://hledger.org/release-notes.html#2024-04-17-hledger-133)

</summary>

### hledger 1.33

### hledger-ui 1.33

### hledger-web 1.33

### project changes 1.33

### credits 1.33

Simon Michael,

[#2133]: https://github.com/simonmichael/hledger/issues/2133

</details>

## Install

At <https://hledger.org/install>, binary packages should be available for this release within a few days (look for green badges). 

Or, you can build from source as described there, after cloning at tag `1.33`:
`git clone https://github.com/simonmichael/hledger --depth 1 -b 1.33`

Or, if under "Assets" below there are release binaries suitable for your OS and hardware, you can use those.
<!--
Note: release binaries have been updated:
- YYYY-MM-DD: description. [#NNNN](https://github.com/simonmichael/hledger/issues/NNNN)
-->
Here are platform-specific instructions for the release binaries.
(You can copy & paste each block of commands as a unit to save time.):

<details>
<summary>

### GNU/Linux on 64-bit Intel

</summary>

At the command line,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.33/hledger-linux-x64.zip   # can rerun if interrupted
unzip hledger-linux-x64.zip; tar xvf hledger-linux-x64.tar; rm hledger-linux-x64.{zip,tar}        # github workaround, preserves permissions
cd -
hledger --version  # should show the new version
touch $HOME/.hledger.journal   # ensure a default journal file exists
```

</details>

<details>
<summary>

### Mac on 64-bit Intel

</summary>

In a terminal window,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.33/hledger-mac-x64.zip
unzip hledger-mac-x64.zip; tar xvf hledger-mac-x64.tar; rm hledger-mac-x64.{zip,tar}              # github workaround, preserves permissions
open .
# for the hledger, hledger-ui, hledger-web icons: right-click, Open, confirm it's ok to run
cd -
hledger --version  # should show the new version
touch $HOME/.hledger.journal   # ensure a default journal file exists
```

</details>

<details>
<summary>

### Windows 64-bit Intel (or ARM, using emulation)

</summary>

In a powershell window (press Windows-r, type powershell, press enter),

1. Make a place to keep hledger binaries, and add it to your PATH; this makes running hledger easier. You only need to do this once, not for every release:
```
mkdir -force $HOME\bin >$null
$ENV:PATH += ";"+$HOME+"\bin"
[Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User)+";"+$HOME+"\bin", [EnvironmentVariableTarget]::User)
```

2. Download and install the release binaries:
```
cd $HOME\bin
curl https://github.com/simonmichael/hledger/releases/download/1.33/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
Expand-Archive hledger-windows-x64.zip -DestinationPath .
rm hledger-windows-x64.zip
cd $HOME
hledger --version           # should show the new version
```

3. Ensure a default journal file exists, and without a problematic encoding. 
(Not sure why "ascii" is needed here - hledger likes utf8 and understands utf8 BOM headers..
but the state of [our unicode support on Windows](https://github.com/simonmichael/hledger/issues?q=is%3Aissue+label%3A%22platform%3A+windows%22+label%3Ai18n)
is really unknown, your input welcome.)
```
out-file -append -encoding ascii $HOME/.hledger.journal
```

Once that journal file exists, you can start hledger-web by double-clicking on the icon if you wish.

</details>

<details>
<summary>

### Windows 7 on 64-bit Intel

</summary>

- click hledger-windows-x64.zip below
- choose Open with Windows Explorer, OK
- click Extract all files
- choose a destination folder - ideally one that appears in `echo %PATH%`, like `C:\Windows` (though that one will require administrator permission); otherwise, your home directory (`C:\Users\YOURNAME`)
- check "Show extracted files when complete"
- click Extract, wait for the destination folder to open
- find the hledger, hledger-web icons (if you extracted to `\Windows`, you'll need to scroll down)
- for each icon: double-click, uncheck "Always ask before opening this file", click Run
- close those Explorer windows
- open a command window (press Windows-r, type CMD, press enter)
- `hledger --version` should show the new version
- `echo # >> .hledger.journal` to ensure a default journal file exists. (Important: the doubled **>>** is needed to avoid overwriting existing data.)

Problems:
- Starting hledger by double-clicking its icon won't work because it needs arguments; run it from the command window instead.
- Starting hledger-web by double-clicking its icon may fail eg because Explorer's command window is too small;
  configure that to be larger, or run hledger-web from a command window instead.
- hledger or hledger-web may fail to run if there is not enough memory available.

</details>

## Next steps

- https://hledger.org/#quick-start

<!-- ## Updates -->
<!-- 2022-06-08: windows-x64 binaries fixed. [#1869](https://github.com/simonmichael/hledger/issues/1869) -->
