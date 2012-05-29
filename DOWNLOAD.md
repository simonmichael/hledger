---
title: hledger downloads
---

# Downloads

<style>
tr.heading td {
    border-top:thin solid #ddd;
}
td {
    vertical-align:top;
    font-size:small;
}
code {
    white-space:nowrap; 
}
</style>

<br>
<a href="https://www.wepay.com/donate/74643?ref=widget&utm_medium=widget&utm_campaign=donation"
   target="_blank" style="float:right;margin:0 1em;"
   ><img src="https://www.wepay.com/img/widgets/donate_with_wepay.png" alt="Donate with WePay" /></a>

Here are some old binaries, all lagging sadly behind the
[current release](NEWS.html).  You can donate to fund the tedious work of
producing a portable up-to-date binary for your platform, which will then
be available to all. This is a quick way to help the project and your
fellow users! If you don't want to do that, you can of course build your
own: see [Installing](../MANUAL.html#installing).

<table>
  <tr>
    <th width="34%" colspan=2><img src="images/linux.png" /><br />GNU/Linux</th>
    <th width="33%"><img src="images/mac.png" border=0 /><br />Mac</th>
    <th width="33%"><img src="images/windows.png" border=0 /><br />Windows</th>
  </tr>
  <tr style="text-align:center; white-space:nowrap;">
    <td width="25%">**32-bit**</td>
    <td width="25%">**64-bit**</td>
    <td width="25%">**10.5**</td>
    <td width="25%"></td>
  </tr>

  <tr class="heading"><td colspan=4>**hledger** (command-line interface):</td></tr>
  <tr align="center">
    <td>
      [hledger-0.14-linux-i686.gz](download/hledger-0.14-linux-i686.gz)<br><br>
    </td>
    <td>
      [hledger-0.14-linux-x86_64.gz](download/hledger-0.14-linux-x86_64.gz)<br><br>
    </td>
    <td>
      [hledger-0.14-mac-i386.gz](download/hledger-0.14-mac-i386.gz)<br><br>
    </td>
    <td>
      [hledger-0.14-windows-i386.exe.zip](download/hledger-0.14-windows-i386.exe.zip)
    </td>
  </tr>

  <!-- <tr><td colspan=4>optional add-ons:</td></tr> -->
  <tr class="heading"><td colspan=4>**hledger-web** (web interface):</td></tr>
  <tr align="center">
    <td>
      [hledger-web-0.14-linux-i686.gz](download/hledger-web-0.14-linux-i686.gz)<br><br>
    </td>
    <td>
      [hledger-web-0.14-linux-x86_64.gz](download/hledger-web-0.14-linux-x86_64.gz)<br><br>
    </td>
    <td>
      [hledger-web-0.14-mac-i386.gz](download/hledger-web-0.14-mac-i386.gz)<br><br>
    </td>
    <td>
      [hledger-web-0.14-windows-i386.exe.zip](download/hledger-web-0.14-windows-i386.exe.zip)
    </td>
  </tr>

  <tr class="heading"><td colspan=4>**hledger-vty** (curses interface):</td></tr>
  <tr align="center">
    <td>
      [hledger-vty-0.14-linux-i686.gz](download/hledger-vty-0.14-linux-i686.gz)<br><br>
    </td>
    <td>
      [hledger-vty-0.14-linux-x86_64.gz](download/hledger-vty-0.14-linux-x86_64.gz)<br><br>
    </td>
    <td>
      [hledger-vty-0.14-mac-i386.gz](download/hledger-vty-0.14-mac-i386.gz)<br><br>
    </td>
    <td>
      not supported
    </td>
  </tr>
  <tr class="heading"><td colspan=4>**hledger-chart** (pie chart generator):</td></tr>
  <tr align="center">
    <td>
      <!-- [hledger-chart-0.14-linux-i686.gz](download/hledger-chart-0.14-linux-i686.gz)<br><br> -->
      &nbsp;
    </td>
    <td>
      <!-- [hledger-chart-0.14-linux-x86_64.gz](download/hledger-chart-0.14-linux-x86_64.gz)<br><br> -->
    </td>
    <td>
      <!-- [hledger-chart-0.14-mac-i386.gz](download/hledger-chart-0.14-mac-i386.gz)<br><br> -->
    </td>
    <td>
    </td>
  </tr>

  <!-- <tr> -->
  <!--   <td colspan=2> -->
  <!--     Open a terminal window and go to your browser's download directory, then:   -->
  <!--     `$ gunzip hledger-*86*`   -->
  <!--     `$ mv hledger-*86* hledger`   -->
  <!--     `$ chmod +x hledger`   -->
  <!--     `$ ./hledger`   -->
  <!--   </td> -->
  <!--   <td> -->
  <!--     Double-click the downloaded file to decompress it.   -->
  <!--     Rename the decompressed file to "hledger".   -->
  <!--     Open a terminal window and go to your browser's download directory, then:   -->
  <!--     `$ chmod +x hledger`   -->
  <!--     Run it:   -->
  <!--     `$ ./hledger` -->
  <!--   </td> -->
  <!--   <td> -->
  <!--     Unzip it to (eg) your desktop.   -->
  <!--     Double-click on the unzipped file to run the web interface (the default behaviour on windows).   -->
  <!--     A security dialog may pop up, where you can choose whether other machines -->
  <!--     may access your hledger web interface. -->
  <!--   </td> -->
  <!-- </tr> -->

</table>

Note you may need to decompress *(eg: gunzip FILE.gz)* and make these
executable *(eg: chmod +x FILE)* after downloading.

