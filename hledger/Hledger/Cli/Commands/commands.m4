m4_dnl _commands_
m4_dnl Used in the hledger manual to include all command docs.
m4_dnl Keep synced with Hledger.Cli.Commands.commandsList, hledger.m4.md
m4_define({{_commands_}}, 
{{
_command_({{Accounts}})
_command_({{Activity}})
_command_({{Add}})
_command_({{Aregister}})
_command_({{Balance}})
_command_({{Balancesheet}})
_command_({{Balancesheetequity}})
_command_({{Cashflow}})
_command_({{Check}})
_command_({{Close}})
_command_({{Codes}})
_command_({{Commodities}})
_command_({{Demo}})
_command_({{Descriptions}})
_command_({{Diff}})
_command_({{Files}})
_command_({{Help}})
_command_({{Import}})
_command_({{Incomestatement}})
_command_({{Notes}})
_command_({{Payees}})
_command_({{Prices}})
_command_({{Print}})
_command_({{Register}})
_command_({{Rewrite}})
_command_({{Roi}})
_command_({{Stats}})
_command_({{Tags}})
_command_({{Test}})
}})m4_dnl
m4_dnl
m4_dnl _command_(MDFILE)
m4_dnl Include a command's doc. The doc should start with a level two heading.
m4_define({{_command_}},
{{
_include_(hledger/Hledger/Cli/Commands/$1.md)
}})m4_dnl
