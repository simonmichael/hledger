<!--
       _              _             _            
  __ _| |__  _ __ ___| |_ __   ___ | |_ ___  ___ 
 / _` | '_ \| '__/ _ \ | '_ \ / _ \| __/ _ \/ __|
| (_| | | | | | |  __/ | | | | (_) | ||  __/\__ \
 \__, |_| |_|_|  \___|_|_| |_|\___/ \__\___||___/
 |___/                                           

-->

## Release notes


### hledger 1.33.1

- process >=1.6.19.0 seems not strictly needed and is no longer required,
  improving installability.
  [#2149]

- `print` and `close` now show a trailing decimal mark on cost amounts also,
  when needed to disambiguate a digit group mark.

- The balance commands' HTML output now includes digit group marks when
  appropriate (fixes a regression in 1.25).
  [#2196]

- The add command no longer shows ANSI escape codes in terminals that
  don't support them.

- Doc updates:
  - import: Skipping -> Date skipping, discuss commodity styles more
  - csv: Amount decimal places: expand, note import behaviour

### hledger-ui 1.33.1

- Require vty-windows-0.2.0.2+ to avoid display problems in recent
  MS Terminal on Windows.

- process >=1.6.19.0 seems not strictly needed and is no longer required,
  improving installability.
  [#2149]

### hledger-web 1.33.1

- Support base64 >=1.0

### credits 1.33.1

- Simon Michael (@simonmichael)

[#2149]: https://github.com/simonmichael/hledger/issues/2149
[#2196]: https://github.com/simonmichael/hledger/issues/2196


<details>
<summary>

## How to install

</summary>

This release may arrive in your local packaging system soon - look for green badges at [hledger.org: Install](https://hledger.org/install.html).
Or you can build it yourself from source, as described on that page.
Or you can use the binaries below:
<!--
Updates to binaries:
- YYYY-MM-DD: description. [#NNNN](https://github.com/simonmichael/hledger/issues/NNNN)
-->

<details>
<summary>

### GNU/Linux on 64-bit Intel

</summary>

At the command line,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.33.1/hledger-linux-x64.zip    # just rerun if interrupted
unzip hledger-linux-x64.zip && tar xvf hledger-linux-x64.tar && rm -f hledger-linux-x64.{zip,tar}  # github workaround, preserves permissions
cd
hledger --version    # should show the new version
```

The tar + zip packaging is a workaround to preserve file permissions.

</details>
<details>
<summary>

### Mac on 64-bit ARM or Intel

</summary>

In a terminal window, run these commands to download, unpack, authorise, and install the binaries in your command line PATH.
Don't use your web browser, it won't authorise the binaries.:
<!--
(Hopefully these commands are all installed by default; 
if not, install [XCode Command Line Tools](https://mac.install.guide/commandlinetools/) 
and/or [Homebrew](https://brew.sh), and let me know.)
-->

```
cd /usr/local/bin

# for ARM macs:
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.33.1/hledger-mac-arm64.zip    # just rerun if interrupted
unzip hledger-mac-arm64.zip && tar xvf hledger-mac-arm64.tar && rm -f hledger-mac-arm64.{zip,tar}  # github workaround, preserves permissions

# or for Intel macs:
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.33.1/hledger-mac-x64.zip
unzip hledger-mac-x64.zip && tar xvf hledger-mac-x64.tar && rm -f hledger-mac-x64.{zip,tar}

cd
hledger --version    # should show the new version
```

The tar + zip packaging is a workaround to preserve file permissions.

</details>
<details>
<summary>

### Windows on 64-bit Intel or ARM

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
cp hledger.exe hledger.old.exe            # keep a backup of the old executables, if you care
cp hledger-ui.exe hledger-ui.old.exe
cp hledger-web.exe hledger-web.old.exe
curl https://github.com/simonmichael/hledger/releases/download/1.33.1/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
Expand-Archive hledger-windows-x64.zip -DestinationPath . -Force
cd $HOME
hledger --version      # should show the new version
hledger-ui --version
hledger-web --version
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

\
Once installed, run `hledger`, and perhaps read [hledger.org: Quick start](https://hledger.org/#quick-start).

</details>
