#!/bin/bash
# Output subcommands from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmp
    tmp=$(mktemp)
    cat > "$tmp"

    # Do not output mistaken commands that start with a dash (e.g. -h)
    sed -rn 's/^ ([-a-z]+).*/\1/gp' "$tmp" \
	| grep -v ^-

    # Output single command aliases in parenthesis:
    # Do not output single letter command aliases, it's not useful.
    sed -rn 's/^ .*\(([a-z]+)\).*/\1/gp' "$tmp" \
	| grep -v ^.$

    # TODO missing: (reg, r)  (multiple aliases)
}

main "$@"
