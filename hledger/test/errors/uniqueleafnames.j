#!/usr/bin/env -S hledger check uniqueleafnames -f

account a:c
account b:c
commodity 1.
payee p

2022/1/1 p
  (a:c)  1

2022/1/1 p
  (b:c)  1
