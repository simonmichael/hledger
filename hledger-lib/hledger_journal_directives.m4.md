<!-- <style> -->
<!-- table a code { white-space:nowrap; } -->
<!-- h1,h2,h3,h4,h5,h6 { color:red; } -->
<!-- </style> -->

### Summary of directives

hledger's directives are based on Ledger's, but there are many differences (and also some between hledger versions).
Directives' behaviour and interactions can get a little bit [complex](https://github.com/simonmichael/hledger/issues/793), 
especially with multiple files,
so here is a table summarising them and their effects.


| directive         | end directive       | subdirectives   | purpose                                                            | can affect (as of 2018/06)
|:------------------|:--------------------|:----------------|:-------------------------------------------------------------------|:---------------------------------------------
| [`account`]       |                     | any text        | declare an account name & optional account code                    | account code: balance reports (except `balance` single-column mode)  <!-- all entries in all files -->
| [`alias`]         | `end aliases`       |                 | rewrite account names                                              | following inline/included entries until end of current file or end directive
| [`apply account`] | `end apply account` |                 | prepend a common parent to account names                           | following inline/included entries until end of current file or end directive
| [`comment`]       | `end comment`       |                 | ignore part of journal                                             | following inline/included entries until end of current file or end directive
| [`commodity`]     |                     | `format`        | declare a commodity and its number notation & display style        | number notation: following entries in that commodity in all files; <br>display style: amounts of that commodity in reports
| [`D`]             |                     |                 | declare a commodity, number notation & display style for commodityless amounts  | commodity: all commodityless entries in all files; <br>number notation: following commodityless entries and entries in that commodity in all files; <br>display style: amounts of that commodity in reports
| [`include`]       |                     |                 | include entries/directives from another file                       | what the included directives affect
| [`P`]             |                     |                 | declare a market price for a commodity                             | amounts of that commodity in reports, when -V is used
| [`Y`]             |                     |                 | declare a year for yearless dates                                  | following inline/included entries until end of current file

[`account`]:       http://hledger.org/journal.html#declaring-accounts
[`alias`]:         http://hledger.org/journal.html#rewriting-accounts
[`apply account`]: http://hledger.org/journal.html#default-parent-account
[`comment`]:       http://hledger.org/journal.html#comment-blocks
[`commodity`]:     http://hledger.org/journal.html#declaring-commodities
[`D`]:             http://hledger.org/journal.html#default-commodity
[`include`]:       http://hledger.org/journal.html#including-other-files
[`P`]:             http://hledger.org/journal.html#market-prices
[`Y`]:             http://hledger.org/journal.html#default-year

#### Definitions:

|||
|:----------------|:--------------------------------------------------------------------------------------------------------------------
| subdirective    | optional indented directive or unparsed text lines immediately following a parent directive
| account code    | numeric code influencing account display order in most balance reports
| number notation | how to interpret numbers when parsing journal entries (the identity of the decimal separator character). Currently each commodity can have its own notation, even in the same file.
| display style   | how to display amounts of a commodity in reports (symbol side and spacing, digit groups, decimal separator, decimal places)
| directive scope | which entries and (when there are multiple files) which files are affected by a directive

<!-- | **entries affected:**  | -->
<!-- | following     | subsequent entries in the file/parse stream -->
<!-- | delimited     | subsequent entries, until an optional end directive -->
<!-- | all           | all preceding and following entries -->
<!-- | **files affected:**    | -->
<!-- | current       | affects current file only -->
<!-- | children      | affects current file and files included by it -->
<!-- | siblings      | affects current file, included files, and other same-level files, but not higher-level files -->
<!-- | all           | affects all files -->

