m4_changequote({{,}})m4_dnl
m4_dnl
m4_define({{_include_}},     m4_defn({{m4_include}})              )m4_dnl
m4_define({{_man_}},         m4_ifdef({{MAN}},{{$1}})             )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEB}},{{$1}})             )m4_dnl
m4_define({{_webseparate_}}, m4_ifdef({{WEB && SEPARATE}},{{$1}}) )m4_dnl
m4_define({{_webcombined_}}, m4_ifdef({{WEB && COMBINED}},{{$1}}) )m4_dnl
m4_dnl
m4_define({{_author_}}, {{}})m4_dnl
m4_define({{_monthyear_}}, {{May 2016}})m4_dnl
m4_define({{_currentrelease_}}, {{0.27}})m4_dnl
m4_define({{_version_}}, {{0.28}})m4_dnl
m4_define({{_versions_}},
<div class="versions">
version:
<a href="$1.html">dev</a>
| <a href="doc/_currentrelease_()/manual.html{{#}}m4_patsubst($1,{{hledger-}})">_currentrelease_()</a>
</div>)m4_dnl
m4_dnl
m4_define({{_toc_}}, {{* toc}})m4_dnl
m4_dnl
m4_dnl _table_({{
m4_dnl | cell1 | cell2 ...
m4_dnl | cell1 | cell2 ...
m4_dnl ...
m4_dnl }})
m4_define({{_table2_}}, {{
|
|-|-$1}})m4_dnl
m4_dnl
m4_define({{_col2_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-6">$1</div>
<div class="col-sm-6">$2</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_define({{_col3_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-4">$1</div>
<div class="col-sm-4">$2</div>
<div class="col-sm-4">$3</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_define({{_shell_}},     {{```shell$1```}}          )m4_dnl
m4_define({{_shellbold_}}, {{```{.shell .bold}$1```}} )m4_dnl
m4_define({{_journal_}},   {{```journal$1```}}        )m4_dnl
m4_define({{_csv_}},       {{```csv$1```}}            )m4_dnl
m4_define({{_rules_}},     {{```rules$1```}}          )m4_dnl
m4_define({{_timeclock_}}, {{```timeclock$1```}}      )m4_dnl
m4_define({{_timedot_}},   {{```timedot$1```}}        )m4_dnl
