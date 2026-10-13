Additional notes about the hledger project's AI usage, complementing ../AI.md.

Related scripts are in ../../Justfile: run `just ai-help`

Files in this directory. The parenthesised ones are not committed.

- ccusage.rules       - for reading `ccusage` output to generate ccusage.journal.
- (ccusage.journal)   - a snapshot of local claude code usage (from ccusage).
- ai.rules            - for importing from ccusage.journal's monthly areg report to ai.journal.
- (aicommits.csv)     - a snapshot of AI usage info from the hledger repo's commit messages.
- aicommits.csv.rules - for importing from aicommits.csv to ai.journal.
- ai.journal          - the project's permanent log of estimated AI usage.

## Usage report

Last updated: 2026-10-12

<!-- generated, do not edit; use hledger --conf hledger.conf -f ai.journal all-html  -->

<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>kt</td><td align="right" class="amount"><span class="amount">748</span></td><td align="right" class="amount"><span class="amount">3371</span></td><td align="right" class="amount"><span class="amount">1889</span></td><td align="right" class="amount"><span class="amount">707</span></td><td align="right" class="amount"><span class="amount">2200</span></td><td align="right" class="amount"><span class="amount">1300</span></td><td align="right" class="amount"><span class="amount">1411</span></td><td align="right" class="amount"><span class="amount">5850</span></td><td align="right" class="amount"><span class="amount">10939</span></td><td align="right" class="amount"><span class="amount">76</span></td><td align="right" class="amount rowtotal"><span class="amount">28491</span></td><td align="right" class="amount rowaverage"><span class="amount">2849</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>Mt</td><td align="right" class="amount"><span class="amount">0.7</span></td><td align="right" class="amount"><span class="amount">3.4</span></td><td align="right" class="amount"><span class="amount">1.9</span></td><td align="right" class="amount"><span class="amount">0.7</span></td><td align="right" class="amount"><span class="amount">2.2</span></td><td align="right" class="amount"><span class="amount">1.3</span></td><td align="right" class="amount"><span class="amount">1.4</span></td><td align="right" class="amount"><span class="amount">5.8</span></td><td align="right" class="amount"><span class="amount">10.9</span></td><td align="right" class="amount"><span class="amount">0.1</span></td><td align="right" class="amount rowtotal"><span class="amount">28.5</span></td><td align="right" class="amount rowaverage"><span class="amount">2.8</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>kWh</td><td align="right" class="amount"><span class="amount">1.9</span></td><td align="right" class="amount"><span class="amount">8.4</span></td><td align="right" class="amount"><span class="amount">4.7</span></td><td align="right" class="amount"><span class="amount">1.8</span></td><td align="right" class="amount"><span class="amount">5.5</span></td><td align="right" class="amount"><span class="amount">3.2</span></td><td align="right" class="amount"><span class="amount">3.5</span></td><td align="right" class="amount"><span class="amount">14.6</span></td><td align="right" class="amount"><span class="amount">27.3</span></td><td align="right" class="amount"><span class="amount">0.2</span></td><td align="right" class="amount rowtotal"><span class="amount">71.2</span></td><td align="right" class="amount rowaverage"><span class="amount">7.1</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>L</td><td align="right" class="amount"><span class="amount">15.0</span></td><td align="right" class="amount"><span class="amount">67.4</span></td><td align="right" class="amount"><span class="amount">37.8</span></td><td align="right" class="amount"><span class="amount">14.1</span></td><td align="right" class="amount"><span class="amount">44.0</span></td><td align="right" class="amount"><span class="amount">26.0</span></td><td align="right" class="amount"><span class="amount">28.2</span></td><td align="right" class="amount"><span class="amount">117.0</span></td><td align="right" class="amount"><span class="amount">218.8</span></td><td align="right" class="amount"><span class="amount">1.5</span></td><td align="right" class="amount rowtotal"><span class="amount">569.8</span></td><td align="right" class="amount rowaverage"><span class="amount">57.0</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>kg</td><td align="right" class="amount"><span class="amount">1.3</span></td><td align="right" class="amount"><span class="amount">6.1</span></td><td align="right" class="amount"><span class="amount">3.4</span></td><td align="right" class="amount"><span class="amount">1.3</span></td><td align="right" class="amount"><span class="amount">4.0</span></td><td align="right" class="amount"><span class="amount">2.3</span></td><td align="right" class="amount"><span class="amount">2.5</span></td><td align="right" class="amount"><span class="amount">10.5</span></td><td align="right" class="amount"><span class="amount">19.7</span></td><td align="right" class="amount"><span class="amount">0.1</span></td><td align="right" class="amount rowtotal"><span class="amount">51.3</span></td><td align="right" class="amount rowaverage"><span class="amount">5.1</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>dryer_hours</td><td align="right" class="amount"><span class="amount">0.4</span></td><td align="right" class="amount"><span class="amount">1.7</span></td><td align="right" class="amount"><span class="amount">0.9</span></td><td align="right" class="amount"><span class="amount">0.4</span></td><td align="right" class="amount"><span class="amount">1.1</span></td><td align="right" class="amount"><span class="amount">0.6</span></td><td align="right" class="amount"><span class="amount">0.7</span></td><td align="right" class="amount"><span class="amount">2.9</span></td><td align="right" class="amount"><span class="amount">5.5</span></td><td align="right" class="amount"><span class="amount">0</span></td><td align="right" class="amount rowtotal"><span class="amount">14.2</span></td><td align="right" class="amount rowaverage"><span class="amount">1.4</span></td></tr>
</table>
<style>
table {border-collapse:collapse}
th, td {padding-left:1em}
th.account, td.account {padding-left:0;}
td.date, span.amount {white-space:nowrap}
.border-left-single {border-left:1px solid}
.border-left-double {border-left:3px double}
.border-right-single {border-right:1px solid}
.border-right-double {border-right:3px double}
.border-top-single {border-top:1px solid}
.border-top-double {border-top:3px double}
.border-bottom-single {border-bottom:1px solid}
.border-bottom-double {border-bottom:3px double}
</style>
<link rel="stylesheet" href="hledger.css">
<h3 class="report-title"> </h3>
<table>
<tr><th class="account border-bottom-double">account</th><th class="border-bottom-double">commodity</th><th class="amount border-bottom-double">2026-01</th><th class="amount border-bottom-double">2026-02</th><th class="amount border-bottom-double">2026-03</th><th class="amount border-bottom-double">2026-04</th><th class="amount border-bottom-double">2026-05</th><th class="amount border-bottom-double">2026-06</th><th class="amount border-bottom-double">2026-07</th><th class="amount border-bottom-double">2026-08</th><th class="amount border-bottom-double">2026-09</th><th class="amount border-bottom-double">2026-10</th><th class="amount rowtotal border-bottom-double">total</th><th class="amount rowaverage border-bottom-double">average</th></tr>
<tr><td class="account">...</td><td>prius_miles</td><td align="right" class="amount"><span class="amount">2.8</span></td><td align="right" class="amount"><span class="amount">12.5</span></td><td align="right" class="amount"><span class="amount">7.0</span></td><td align="right" class="amount"><span class="amount">2.6</span></td><td align="right" class="amount"><span class="amount">8.1</span></td><td align="right" class="amount"><span class="amount">4.8</span></td><td align="right" class="amount"><span class="amount">5.2</span></td><td align="right" class="amount"><span class="amount">21.6</span></td><td align="right" class="amount"><span class="amount">40.5</span></td><td align="right" class="amount"><span class="amount">0.3</span></td><td align="right" class="amount rowtotal"><span class="amount">105.4</span></td><td align="right" class="amount rowaverage"><span class="amount">10.5</span></td></tr>
</table>
