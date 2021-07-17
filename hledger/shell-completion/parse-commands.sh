#!/usr/bin/env bash
# Parse hledger's help and output all commands and command aliases in
# parenthesis. Do not output single letter command aliases, it's not useful.
set -euo pipefail

declare commands_help
commands_help=$(hledger)
{
    sed -rn 's/^[[:space:]]+([a-z][-a-z]+)[[:space:]]+.*/\1/p' <<< "$commands_help"
    sed -rn 's/^[[:space:]]+[a-z][-a-z]+[[:space:]]+\(([a-z][ ,a-z]+)\).*/\1/p' <<< "$commands_help" |
    sed 's/[[:space:]]*,[[:space:]]*/\n/g' |
    sed '/^.$/d'
} | sed '/^hledger/d' | sort -u

# Local Variables:
# mode: sh
# sh-basic-offset: 4
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et
