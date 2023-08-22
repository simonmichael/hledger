#!/usr/bin/env -S hledger check balancednoautoconversion -f
# Show the error when balancedwithautoconversion is required
# and an implicit commodity conversion is found.
# Currently the same as the regular balancedwithautoconversion error.

2022/1/1
  a   1 A
  b  -1 B
