#!/bin/bash
# Output subcommands from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmp="_commands.tmp"
    cat > "$tmp"

    sed -rn 's/^\s+([a-z][-a-z]+)\s+.*/\1/p' "$tmp"

    # Output command aliases in parenthesis:
    # Do not output single letter command aliases, it's not useful.
    sed -rn 's/^\s+[a-z][-a-z]+\s+\(([a-z][ ,a-z]+)\).*/\1/p' "$tmp" |
        sed 's/\s*,\s*/\n/g' |
        sed '/^.$/d'
}

main "$@"
