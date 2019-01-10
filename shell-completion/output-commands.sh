#!/bin/bash
# Output subcommands from man/usage text

set -o errexit -o pipefail -o nounset

printCommands() {
    declare tmp=$1
    sed -rn 's/^ ([-a-z]+).*/\1/gp' "$tmp"
    sed -rn 's/^ .*\(([a-z]+)\).*/\1/gp' "$tmp"
    # TODO missing: (reg, r)  (multiple aliases)
}

main() {
    declare tmp
    tmp=$(mktemp)
    cat > "$tmp"

    printCommands "$tmp" | grep -v ^hledger
}

main "$@"
