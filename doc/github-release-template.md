<details>
<summary>

## Release notes (https://hledger.org/release-notes.html#hledger-A-BB)

</summary>

## YYYY-MM-DD hledger-A.BB

**Summary.**
<!-- ([announcement](https://groups.google.com/g/hledger/LINK)) -->

### hledger A.BB

<!-- etc, from release-notes.md -->

</details>

## Installing

At <https://hledger.org/install>, binary packages should be available for this release within a few days (look for green badges). 

Or, you can build from source as described there, after cloning at tag `A.BB`:
`git clone https://github.com/simonmichael/hledger --depth 1 -b A.BB`

Or, if under "Assets" below there are release binaries suitable for your OS and hardware, you can use those.
<!--
(Note, release binaries have been updated:
- YYYY-MM-DD: description. [#NNNN](https://github.com/simonmichael/hledger/issues/NNNN)
-->
Here are platform-specific instructions for the release binaries.
(You can copy & paste each block of commands as a unit, to save time.):

<details>
<summary>

### GNU/Linux on 64-bit Intel
</summary>
At the command line,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/A.BB/hledger-linux-x64.zip   # can rerun this if interrupted
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
curl -LOC- https://github.com/simonmichael/hledger/releases/download/A.BB/hledger-mac-x64.zip
unzip hledger-mac-x64.zip; tar xvf hledger-mac-x64.tar; rm hledger-mac-x64.{zip,tar}              # github workaround, preserves permissions
open .
# for the hledger, hledger-ui, hledger-web icons: right-click the executable, Open, confirm it's ok to run
cd -
hledger --version  # should show the new version
touch $HOME/.hledger.journal   # ensure a default journal file exists
```
</details>

<details>
<summary>

### Windows on 64-bit Intel
</summary>

In a powershell window (press Windows-r, type powershell, press enter),

Make a place to keep hledger binaries, and add it to your PATH; this makes running hledger easier. You only need to do this once, not for every release:
```
mkdir -force $HOME\bin >$null
$ENV:PATH += ";"+$HOME+"\bin"
[Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User)+";"+$HOME+"\bin", [EnvironmentVariableTarget]::User)
```
Download and install the release binaries:
```
cd $HOME\bin
curl https://github.com/simonmichael/hledger/releases/download/A.BB/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
Expand-Archive hledger-windows-x64.zip -DestinationPath .
rm hledger-windows-x64.zip
cd $HOME
hledger --version           # should show the new version
```
And ensure a default journal file exists:
```
out-file -append -encoding ascii $HOME/.hledger.journal
```

Problems:
- Starting hledger/hledger-web by double-clicking their icon won't work; run them from a cmd or powershell window instead.
</details>

<details>
<summary>

### Windows 7 on 64-bit Intel, using Firefox
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
Once installed, you could try these quick starts / tutorials:

- https://hledger.org/index.html#how-to-get-started
- https://hledger.org/add.html
- https://hledger.org/ui.html
- https://hledger.org/web.html


<!--
## Updates
2022-06-05: linux-x64 binaries updated to run at normal speed. [#1867](https://github.com/simonmichael/hledger/issues/1867)
2022-06-08: windows-x64 binaries fixed. [#1869](https://github.com/simonmichael/hledger/issues/1869)
-->
