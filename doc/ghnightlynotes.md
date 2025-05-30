Here you can find unreleased "nightly" binaries, if any. By using these and providing feedback, eg in [chat](https://hledger.org/support.html), you will be helping to make the next hledger release better.

Here's what's new since hledger 1.42: [commits](https://github.com/simonmichael/hledger/compare/1.42...nightly)

<details>
<summary>

## Install

</summary>

<xdetails>
<summary>

### All platforms

</summary>

If you have [eget](https://github.com/zyedidia/eget), that's a convenient way to download the right binaries for your machine:
```
eget simonmichael/hledger --pre-release --all
```
<!-- eget simonmichael/hledger --tag nightly --all -->

Otherwise:

</xdetails>
<xdetails>
<summary>

### GNU/Linux, 64-bit Intel

</summary>

At the command line:

```
cd /usr/local/bin
curl -fLOC- https://github.com/simonmichael/hledger/releases/download/refs/tags/1.42.99/hledger-linux-x64.tar.gz
tar xzf hledger-linux-x64.tar.gz
cd
hledger --version; hledger-ui --version; hledger-web --version    # should show a recent .99 version
```

</xdetails>
<xdetails>
<summary>

### Mac, 64-bit ARM or Intel

</summary>

In a terminal window (don't use your web browser to download, it won't authorise the binaries):
<!--
(Hopefully these commands are all installed by default; 
if not, install [XCode Command Line Tools](https://mac.install.guide/commandlinetools/) 
and/or [Homebrew](https://brew.sh), and let me know.)
-->

For ARM macs:
  ```
  cd /usr/local/bin
  curl -fLOC- https://github.com/simonmichael/hledger/releases/download/refs/tags/1.42.99/hledger-mac-arm64.tar.gz
  tar xzf hledger-mac-arm64.tar.gz
  cd
  hledger --version; hledger-ui --version; hledger-web --version    # should show a recent .99 version
  ```

For Intel macs:
  ```
  cd /usr/local/bin
  curl -fLOC- https://github.com/simonmichael/hledger/releases/download/refs/tags/1.42.99/hledger-mac-x64.tar.gz
  tar xzf hledger-mac-x64.tar.gz
  cd
  hledger --version; hledger-ui --version; hledger-web --version    # should show a recent .99 version
  ```

</xdetails>
<xdetails>
<summary>

### Windows, 64-bit ARM or Intel

</summary>

In a powershell window (press `WINDOWS-R`, `powershell`, `ENTER`):

1. Make a place to keep installed binaries. You only need to do this once, not for every release:
    ```
    mkdir -force $HOME\bin >$null
    $ENV:PATH += ";"+$HOME+"\bin"
    [Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User)+";"+$HOME+"\bin", [EnvironmentVariableTarget]::User)
    ```

2. Download and install the release binaries:
    ```
    cd $HOME\bin
    curl https://github.com/simonmichael/hledger/releases/download/refs/tags/1.42.99/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
    Expand-Archive hledger-windows-x64.zip -DestinationPath . -Force
    cd $HOME
    hledger --version; hledger-ui --version; hledger-web --version    # should show refs/tags/1.42.99; if not, check why: where.exe hledger
    ```

3. Ensure a default journal file exists, and without a problematic encoding (I'm not sure if/why "ascii" was needed here).
This will allow you to start hledger-web by double-clicking on its icon if you wish.
    ```
    out-file -append -encoding ascii $HOME/.hledger.journal
    ```

</xdetails>
<xdetails>

</details>
