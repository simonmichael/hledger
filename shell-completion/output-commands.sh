#!/bin/bash
# Output subcommands from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmp
    tmp=$(mktemp)
    cat > "$tmp"

    sed -rn 's/^ ([-a-z]+).*/\1/gp' "$tmp"

    # Do not output single letter commands, it's not useful.
    sed -rn 's/^ .*\(([a-z]+)\).*/\1/gp' "$tmp" \
	| grep -v ^.$

    # TODO missing: (reg, r)  (multiple aliases)
}

main "$@"
