m4_dnl _commands_(HEADINGHASHES)
m4_dnl Used in the hledger manual to include all command docs.
m4_dnl Keep synced with Hledger.Cli.Commands.commandsList, hledger.m4.md
m4_define({{_commands_}}, 
{{
_command_({{$1 accounts}}           ,{{Accounts}})
_command_({{$1 activity}}           ,{{Activity}})
_command_({{$1 add}}                ,{{Add}})
_command_({{$1 aregister}}          ,{{Aregister}})
_command_({{$1 balance}}            ,{{Balance}})
_command_({{$1 balancesheet}}       ,{{Balancesheet}})
_command_({{$1 balancesheetequity}} ,{{Balancesheetequity}})
_command_({{$1 cashflow}}           ,{{Cashflow}})
_command_({{$1 check}}              ,{{Check}})
_command_({{$1 close}}              ,{{Close}})
_command_({{$1 codes}}              ,{{Codes}})
_command_({{$1 commodities}}        ,{{Commodities}})
_command_({{$1 descriptions}}       ,{{Descriptions}})
_command_({{$1 diff}}               ,{{Diff}})
_command_({{$1 files}}              ,{{Files}})
_command_({{$1 help}}               ,{{Help}})
_command_({{$1 import}}             ,{{Import}})
_command_({{$1 incomestatement}}    ,{{Incomestatement}})
_command_({{$1 notes}}              ,{{Notes}})
_command_({{$1 payees}}             ,{{Payees}})
_command_({{$1 prices}}             ,{{Prices}})
_command_({{$1 print}}              ,{{Print}})
_command_({{$1 print-unique}}       ,{{Printunique}})
_command_({{$1 register}}           ,{{Register}})
_command_({{$1 register-match}}     ,{{Registermatch}})
_command_({{$1 rewrite}}            ,{{Rewrite}})
_command_({{$1 roi}}                ,{{Roi}})
_command_({{$1 stats}}              ,{{Stats}})
_command_({{$1 tags}}               ,{{Tags}})
_command_({{$1 test}}               ,{{Test}})
}})m4_dnl
m4_dnl
m4_dnl _command_(NAME, MDFILE)
m4_dnl Create a command heading and include its doc.
m4_define({{_command_}},
{{$1
_include_(hledger/Hledger/Cli/Commands/$2.md)
}})m4_dnl
