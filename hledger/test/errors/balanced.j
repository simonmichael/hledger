#!/usr/bin/env -S hledger check balanced -f
# Show the error when explicit conversion is required ("balanced")
# but an implicit commodity conversion is found.

2022/1/1
  a   1 A
  b  -1 B
