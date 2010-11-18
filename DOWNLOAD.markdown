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

Here are hledger binaries which should "just work" on your computer.  For
more help and alternatives, please see the <a
href="../MANUAL.html#installing">installing doc</a>.
    
Errata: these binaries lack support files for the web interface, you must copy
  [those files](http://joyful.com/repos/hledger/hledger-web/data/static/) to
  `./data/web/static/` below the directory where you run hledger.

<table id="platformdocs">
	<tr>
		<th><img src="../linux.png" /><br />GNU/Linux (x86)</th>
		<th><a href="download/hledger-0.12.1-mac-i386.gz"><img src="../mac.png" border=0 /></a><br />Mac</th>
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

