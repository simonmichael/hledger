---
title: hledger downloads
---

# hledger downloads

<style>
#platformdocs td {
    width:33%;
    vertical-align:top;
    font-size:small;
}
#platformdocs code {
    white-space:nowrap; 
}
</style>

Here are hledger binaries which might just work on your computer.
Or, you could try <a href="http://hledger.org/MANUAL.html#installing">installing with cabal</a>.
Please report any trouble (or success!) to <a href="http://hledger.org/DEVELOPMENT.html#support">Support</a>.
    
<table id="platformdocs">
	<tr>
		<th><img src="../linux.png" /><br />GNU/Linux (x86)</th>
		<th><a href="download/hledger-0.12.1-mac-i386.gz"><img src="../mac.png" border=0 /></a><br />Mac (intel)</th>
		<th><a href="download/hledger-0.12.1-win-i386.gz"><img src="../windows.png" border=0 /></a><br />Windows</th>
	</tr>
	<tr>
		<td>
Download the  
**[32-bit](download/hledger-0.12.1-linux-i386.gz)** or 
**[64-bit](download/hledger-0.12.1-linux-x86_64.gz)** version  
  
Open a terminal window and go to your browser's download directory, then:  
`$ gunzip hledger-*86*`
`$ mv hledger-*86* hledger`  
`$ chmod +x hledger`  
`$ ./hledger`
		</td>
		<td>
Download  
<span style="white-space:nowrap;">
**[hledger-0.12.1-mac-i386.gz](download/hledger-0.12.1-mac-i386.gz)**  
</span>
  
Double-click the downloaded file to decompress it.  
Rename the decompressed file to "hledger".  
Open a terminal window and go to your browser's download directory, then:  
`$ chmod +x hledger`  
Run it:  
`$ ./hledger`
		</td>
		<td>
Download  
<span style="white-space:nowrap;">
**[hledger-0.12.1-windows-i386.exe.zip](download/hledger-0.12.1-windows-i386.exe.zip)**  
</span>
  
Unzip it to (eg) your desktop.  
Double-click on the unzipped file to run the web interface (the default behaviour on windows).  
A security dialog may pop up, where you can choose whether other machines
may access your hledger web interface.
		</td>
	</tr>
</table>

Errata:

- the binaries have no stylesheet for the web interface unless you [copy it](http://joyful.com/repos/hledger/data/web/style.css) to `./data/web/style.css`
