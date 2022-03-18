# hledger check uniqueleafnames

account a:c  ; XXX check uniqueleafnames doesn't notice these
account b:c  ;
commodity 1.
payee p

1/1 p
  (a:c)  1

1/1 p
  (b:c)  1
