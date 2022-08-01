#!/usr/bin/env -S hledger check recentassertions -f
# Latest balance assertion more than 7 days behind latest posting.

2022-01-01 *
  a  0 = 0

2022-01-09 *
  a  0
