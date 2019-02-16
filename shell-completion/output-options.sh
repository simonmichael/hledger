#!/bin/bash
# Output short and long options from man/usage text

set -o errexit -o pipefail -o nounset

main() {
    declare tmp
    tmp=$(mktemp)
    cat > "$tmp"
    sed -rn 's/.* (-[a-zA-Z0-9]).*/\1/gp' < "$tmp"

    # Do not print '=' after long options with arg because it makes completion
    # for option arguments harder.
    sed -rn 's/.* (--[a-zA-Z][-_a-zA-Z0-9]*)=?.*/\1/gp' < "$tmp"
}

main "$@"
