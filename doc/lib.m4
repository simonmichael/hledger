m4_changequote({{,}})m4_dnl
m4_define({{_man_}}, m4_ifdef({{MAN}},{{$1}}))m4_dnl
m4_define({{_web_}}, m4_ifdef({{WEB}},{{$1}}))m4_dnl
m4_define({{_include_}}, m4_defn({{m4_include}}))m4_dnl
