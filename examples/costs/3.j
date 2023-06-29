2022-01-01
  assets:dollars      $-135
  equity:conversion    $135
  equity:conversion   €-100
  assets:euros         €100

comment

$ hledger -f- print -x --infer-cost --infer-equity
2022-01-01
    assets:dollars       $-135 @@ €100
    equity:conversion             $135
    equity:conversion            €-100
    assets:euros                  €100

>=

$ hledger -f- print -x --infer-cost --infer-equity | hledger -f- bal -c '$1.'
               $-135  assets:dollars
                €100  assets:euros
                $135
               €-100  equity:conversion
--------------------
                   0  
