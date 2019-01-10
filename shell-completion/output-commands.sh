#!/bin/bash
# Output subcommands from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmp
    tmp=$(mktemp)
    cat > "$tmp"

    sed -rn 's/^ ([-a-z]+).*/\1/gp' "$tmp"
    sed -rn 's/^ .*\(([a-z]+)\).*/\1/gp' "$tmp"
    # TODO missing: (reg, r)  (multiple aliases)
}

main "$@"
