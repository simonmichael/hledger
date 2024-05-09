m4_dnl _commands_
m4_dnl Used in the hledger manual to include all command docs.
m4_dnl Keep synced with Hledger.Cli.Commands.commandsList, hledger.m4.md.
m4_dnl The spaced colons avoid anchor/link collisions with similarly-named sections.
m4_define({{_commands_}}, 
{{
## HELP :

_command_({{Help}})
_command_({{Demo}})

## USER INTERFACES :

Alternate user interfaces like
[hledger-ui](hledger-ui.md) and
[hledger-web](hledger-web.md)
appear in this section of the : list when installed.

## ENTERING DATA :

_command_({{Add}})
_command_({{Import}})

## BASIC REPORTS :

_command_({{Accounts}})
_command_({{Codes}})
_command_({{Commodities}})
_command_({{Descriptions}})
_command_({{Files}})
_command_({{Notes}})
_command_({{Payees}})
_command_({{Prices}})
_command_({{Stats}})
_command_({{Tags}})

## STANDARD REPORTS :

_command_({{Print}})
_command_({{Aregister}})
_command_({{Register}})
_command_({{Balancesheet}})
_command_({{Balancesheetequity}})
_command_({{Cashflow}})
_command_({{Incomestatement}})

## ADVANCED REPORTS :

_command_({{Balance}})
_command_({{Roi}})

## CHARTS :

_command_({{Activity}})

## GENERATING DATA :

_command_({{Close}})
_command_({{Rewrite}})

## MAINTENANCE :

_command_({{Check}})
_command_({{Diff}})
_command_({{Test}})

}})m4_dnl
m4_dnl
m4_dnl _command_(MDFILE)
m4_dnl Include a command's doc. The doc should start with a level two heading.
m4_define({{_command_}},
{{
_include_(hledger/Hledger/Cli/Commands/$1.md)
}})m4_dnl
