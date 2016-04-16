m4_changequote({{,}})m4_dnl
m4_define({{_man_}}, m4_ifdef({{MAN}},{{$1}}))m4_dnl
m4_define({{_web_}}, m4_ifdef({{WEB}},{{$1}}))m4_dnl
m4_define({{_include_}}, m4_defn({{m4_include}}))m4_dnl
m4_define({{_col2_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-6">$1</div>
<div class="col-sm-6">$2</div>
</div>
</div>}})m4_dnl
m4_define({{_col3_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-4">$1</div>
<div class="col-sm-4">$2</div>
<div class="col-sm-4">$3</div>
</div>
</div>}})m4_dnl
m4_define({{_shell_}}, {{```{.shell .clear}$1```}})m4_dnl
m4_define({{_shellbold_}}, {{```{.shell .bold .clear}$1```}})m4_dnl
m4_define({{_journal_}}, {{```{.journal .clear}$1```}})m4_dnl
m4_define({{_currentrelease_}}, {{0.27}})m4_dnl
m4_define({{_versions_}},
{{<div class="versions">
version:
<a href="$1.html">dev</a>
| <a href="_currentrelease_()/$1.html">_currentrelease_()</a>
</div>}})m4_dnl
