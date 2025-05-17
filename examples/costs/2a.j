2022-01-01
  assets:dollars  $-135
  assets:euros     €100 @ $1.35

comment

$ hledger -f- print -x --infer-cost --infer-equity
2022-01-01
    assets:dollars                    $-135
    assets:euros               €100 @ $1.35
    equity:conversion:$-€:€           €-100
    equity:conversion:$-€:$         $135.00

>=

$ hledger -f- print -x --infer-cost --infer-equity | hledger -f- bal -c '$1.'
               $-135  assets:dollars
                €100  assets:euros
                $135  equity:conversion:$-€:$
               €-100  equity:conversion:$-€:€
--------------------
                   0  
