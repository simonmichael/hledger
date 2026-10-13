Additional notes about the hledger project's AI usage, complementing ../AI.md.

Related scripts are in ../../Justfile: run `just ai-help`

Files in this directory. The parenthesised ones are not committed.

- ccusage.rules       - for reading `ccusage` output to generate ccusage.journal.
- (ccusage.journal)   - a snapshot of local claude code usage (from ccusage).
- ai.rules            - for importing from ccusage.journal's monthly areg report to ai.journal.
- (aicommits.csv)     - a snapshot of AI usage info from the hledger repo's commit messages.
- aicommits.csv.rules - for importing from aicommits.csv to ai.journal.
- ai.journal          - the project's permanent log of estimated AI usage.

## Usage report

Here is the estimated AI usage for hledger development so far,
measured in generic output kilotokens (mostly opus 5.x).

The same data is shown in megatokens, and then converted to several other commodities
using ballpark conversion rates,
to give a rough idea of the associated cost in resources -
energy, water, kilograms of CO<sub>2</sub> emitted -
and of the equivalent number of
hours running a tumble dryer,
miles driven in a prius,
etc.
See ai.journal for more detail on these calculations.

<!-- generated with just ai-aij-readme -->

Last updated: 2026-10-12

```
     || Commodity  Jan   Feb   Mar  Apr   May   Jun   Jul   Aug    Sep  Oct    Total  Average 
=====++=======================================================================================
 ... || kt         748  3371  1889  707  2200  1300  1411  5850  10939   76    28491     2849 
 

     || Commodity  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug   Sep  Oct    Total  Average 
=====++================================================================================
 ... || Mt         0.7  3.4  1.9  0.7  2.2  1.3  1.4  5.8  10.9  0.1     28.5      2.8 
 

     || Commodity  Jan  Feb  Mar  Apr  May  Jun  Jul   Aug   Sep  Oct    Total  Average 
=====++=================================================================================
 ... || kWh        1.9  8.4  4.7  1.8  5.5  3.2  3.5  14.6  27.3  0.2     71.2      7.1 
 

     || Commodity   Jan   Feb   Mar   Apr   May   Jun   Jul    Aug    Sep  Oct    Total  Average 
=====++==========================================================================================
 ... || L          15.0  67.4  37.8  14.1  44.0  26.0  28.2  117.0  218.8  1.5    569.8     57.0 
 

     || Commodity  Jan  Feb  Mar  Apr  May  Jun  Jul   Aug   Sep  Oct    Total  Average 
=====++=================================================================================
 ... || kg         1.3  6.1  3.4  1.3  4.0  2.3  2.5  10.5  19.7  0.1     51.3      5.1 
 

     || Commodity    Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct    Total  Average 
=====++=================================================================================
 ... || dryer_hours  0.4  1.7  0.9  0.4  1.1  0.6  0.7  2.9  5.5    0     14.2      1.4 
 

     || Commodity    Jan   Feb  Mar  Apr  May  Jun  Jul   Aug   Sep  Oct    Total  Average 
=====++====================================================================================
 ... || prius_miles  2.8  12.5  7.0  2.6  8.1  4.8  5.2  21.6  40.5  0.3    105.4     10.5 
```
