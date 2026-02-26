
## hledger manual

add a quick reference section near the top ?
is it feasible to have one page per major section ?

## cost/price naming

- cost -> price, cost basis -> basis and cost ?
     Amount{acost,acostbasis} -> Amount{aprice,abasis}
     CostBasis{cbcost} -> Basis{bcost}
  --cost/--basis/-B, --price/-P, --value/-V/--market/--exchange/-X ?
  --infer-costs
  --infer-market-prices
  P directive
  cost and price are both unavoidably generic (price more so because less directional)
  basis price/cost, acquisition price/cost, disposal price/sale amount, market price
  Cost Reporting, Transacted Value Reporting, Market Value Reporting
- enable --infer-costs/equity/market-prices always ?
- clean up old --exchange, --market flags ?
- add --value types: tx (may exist as "cost"), basis ?

