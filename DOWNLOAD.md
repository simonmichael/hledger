---
title: hledger downloads
---

# Downloads

<style>
table {
    margin:2em 0;
}
tr.heading td {
    border-top:thin solid #ddd;
}
td {
    vertical-align:top;
    font-size:small;
}
td a:link {
   color:#888;
}
td strong a:link {
   color:#000;
}
</style>

<br>
<a href="https://www.wepay.com/donate/hledger?ref=widget&utm_medium=widget&utm_campaign=donation"
   target="_blank" style="float:right;margin:1em;"
   ><img src="https://www.wepay.com/img/widgets/donate_with_wepay.png" alt="Donate with WePay" /></a>

Generating and supporting portable binaries costs time and effort that can
be better used elsewhere. So, I do it when stimulated by donations.
Binaries funded in this way will then be available to all - a quick way to
help the project and your fellow users!

Our latest available platform binaries are below. On some platforms, such
as Debian, your packaging system will have more up-to-date and convenient
ones.  Otherwise, you can of course build your own: see
[Installing](../MANUAL.html#installing).

<table>
  <tr>
    <th width="40%" colspan=2><img src="images/linux.png" /><br />GNU/Linux</th>
    <th width="40%" colspan=2><img src="images/mac.png" border=0 /><br />Mac</th>
    <th width="20%"><img src="images/windows.png" border=0 /><br />Windows</th>
  </tr>
  <tr style="text-align:center; white-space:nowrap;">
    <td width="20%">**32-bit**</td>
    <td width="20%">**64-bit**</td>
    <td width="20%">**10.5**</td>
    <td width="20%">**10.7**</td>
    <td width="20%"></td>
  </tr>
  <tr style="text-align:center;color:green;">
    <td width="20%"></td>
    <td width="20%"></td>
    <td width="20%"></td>
    <td width="20%">Sponsor: **Jon&nbsp;Hancock**!</td>
    <td width="20%"></td>
  </tr>

  <tr class="heading"><td colspan=5>**hledger** (command-line):</td></tr>
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
      **[hledger-0.18.2-mac-x86_64.gz](download/hledger-0.18.2-mac-x86_64.gz)**<br><br>
    </td>
    <td>
      [hledger-0.14-windows-i386.exe.zip](download/hledger-0.14-windows-i386.exe.zip)
    </td>
  </tr>

  <!-- <tr><td colspan=4>optional add-ons:</td></tr> -->
  <tr class="heading"><td colspan=5>**hledger-web** (web):</td></tr>
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
      **[hledger-web-0.18.2-mac-x86_64.gz](download/hledger-web-0.18.2-mac-x86_64.gz)**<br><br>
    </td>
    <td>
      [hledger-web-0.14-windows-i386.exe.zip](download/hledger-web-0.14-windows-i386.exe.zip)
    </td>
  </tr>

  <tr class="heading"><td colspan=5>**hledger-vty** (curses):</td></tr>
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
    </td>
    <td>
      not supported
    </td>
  </tr>

  <tr class="heading"><td colspan=5></td></tr>

  <!-- <tr class="heading"><td colspan=5>**hledger-chart** (pie chart generator):</td></tr> -->
  <!-- <tr align="center"> -->
  <!--   <td> -->
  <!--     <\!-- [hledger-chart-0.14-linux-i686.gz](download/hledger-chart-0.14-linux-i686.gz)<br><br> -\-> -->
  <!--     &nbsp; -->
  <!--   </td> -->
  <!--   <td> -->
  <!--     <\!-- [hledger-chart-0.14-linux-x86_64.gz](download/hledger-chart-0.14-linux-x86_64.gz)<br><br> -\-> -->
  <!--   </td> -->
  <!--   <td> -->
  <!--     <\!-- [hledger-chart-0.14-mac-i386.gz](download/hledger-chart-0.14-mac-i386.gz)<br><br> -\-> -->
  <!--   </td> -->
  <!--   <td> -->
  <!--   </td> -->
  <!--   <td> -->
  <!--   </td> -->
  <!-- </tr> -->

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

After downloading you may need/want to decompress, make executable, and/or rename. Eg:

    gunzip hledger-web-0.18.2-mac-x86_64.gz
    chmod +x hledger-web-0.18.2-mac-x86_64
    mv hledger-web-0.18.2-mac-x86_64 /usr/local/bin/hledger-web
