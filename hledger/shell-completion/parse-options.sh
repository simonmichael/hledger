#!/usr/bin/env bash
# Parse hledger's help and output long options. Do not propose single letter
# completions. Options requiring an argument make that explicit by appending the
# equal sign (=)
set -euo pipefail

declare subcommand=${1:-}
declare hledgerArgs=(--help)
[[ -n $subcommand ]] && hledgerArgs=("$subcommand" "${hledgerArgs[@]}")

hledger "${hledgerArgs[@]}" |
    sed -rn '/^[[:space:]]+-/p' |
    sed -rn 's/^[[:space:]]{1,4}(-.)?[[:space:]]{1,4}(--[a-zA-Z][-_a-zA-Z0-9]+=?).*/\2/p' |
    sort -u

# Local Variables:
# mode: sh
# sh-basic-offset: 4
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et
