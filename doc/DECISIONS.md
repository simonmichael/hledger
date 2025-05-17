# Decisions

A partial list of notable development decisions.

## 2022

### Replace "transaction price" terminology with "cost"

"Transaction price" never quite stuck. "Cost" is simpler, shorter, more intuitive, consistent with `--cost` and "cost reporting", and more distinct from "market price".

There is an (acceptable) ambiguity: "cost" could mean the `@ UNITCOST` price attached to the amount, or the total cost when the amount is converted (`QUANTITY * UNITCOST`).

Status: as of 2023Q1 this has been done in the manuals and is slowly ongoing in the code.

## 2023

### Plugin types

We will document and support where feasible several distinct kinds of plugin, written in haskell or other languages,
such as reader, processor, writer, formatter, command. See <https://hledger.org/scripting.html#plugin-types>.
