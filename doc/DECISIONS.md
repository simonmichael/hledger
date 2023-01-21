# Decisions

A partial list of notable development decisions.

## 2022

### Replace "transaction price" terminology with "cost"

"Transaction price" never quite stuck. "Cost" is simpler, shorter, more intuitive, consistent with `--cost` and "cost reporting", and more distinct from "market price".

There is an (acceptable) ambiguity: "cost" could mean the `@ UNITCOST` price attached to the amount, or the total cost when the amount is converted (`QUANTITY * UNITCOST`).

Status: as of 2023Q1 this has been done in the manuals and is slowly ongoing in the code.

## 2023

### Plugin types

We will document and support where feasible several distinct kinds of plugin, written in haskell or other languages:

| Plugin type | Description | Current status |
|-|-|-|
| reader    | Parse/ingest new data formats/sources to hledger journal data.          | Can be done by generating journal or csv format.
| writer    | Render/export hledger journal data to new data formats/destinations.    | Can be done (lossily) by transforming print's txt/csv/json/sql output.
| processor | Process hledger journal data after parsing, before reporting.           | Not well supported, somewhat possible in a haskell script.
| formatter | Render a hledger report's output to a new output format.                | Can be done by transforming report's builtin txt/csv/json/html output.
| command   | Provide new `hledger` subcommands implementing new reports or actions.  | Can be done with addon scripts/programs in PATH.
