#!/bin/bash
# Output short and long options from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmpdir="_options.tmp"
    declare tmp="${tmpdir}/${1:-generic}"

    mkdir -p "$tmpdir"
    cat > "$tmp"

    # Do not propose single letter completions. It's not useful, it's noisy
    # and it makes completion slower:
    # Display all 200 possibilities? (y or n)
    # sed -rn 's/.* (-[a-zA-Z0-9]).*/\1/gp' < "$tmp"

    # Options requiring an argument make that explicit by appending
    # the equal sign (=)
    sed -rn '/^\s+-/p' "$tmp" |
        sed -rn 's/^\s{1,4}(-.)?\s{1,4}(--[a-zA-Z][-_a-zA-Z0-9]+=?).*/\2/p'
}

main "$@"
