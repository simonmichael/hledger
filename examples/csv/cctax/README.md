## Cryptocurrency tax reporting

<div class=pagetoc>

<!-- toc -->
</div>

(This is the README in the hledger repo's `examples/csv/cctax/` directory,
also published as the [Cryptocurrency tax reporting] page on hledger.org.)
<!-- This page can be viewed on github or hledger.org, so use absolute urls -->
[Cryptocurrency tax reporting]: https://hledger.org/cctax.html

In this directory we are not concerned with importing to hledger,
but with exporting from hledger to cryptocurrency tax calculators.

Why do this ? In most countries, the revenue and capital gain or loss incurred from
receiving/selling/trading cryptocurrencies is taxable and must be itemised and reported.
There are several aspects to this:

1. Calculating income.
2. Calculating capital gains. 
3. Calculating actual taxes owed.

1 (income) is relatively easy to do with hledger.
3 (tax owed) requires knowledge of current tax rules and is often handled by specialised tax preparation software.
2 (gains) can be complex. Depending on your country's tax rules, it may require
- tracking the original cost, date, and wallet of every purchase/acquisition over your lifetime
- tracking their movements/splits/merges
- selling/disposing them in a precise mandated order
- and thereby calculating the capital gains or losses.

Centralised exchanges will calculate gains for you, 
but self-custodied wallets, decentralised exchanges, or other defi apps won't;
there you are responsible for calculating gains.
There are several ways you could tackle this:

1. Calculate gains using only hledger,
   by keeping track of each lot and lot movement with subaccounts,
   and selecting the appropriate lots/costs in disposal transactions.
   This is intuitive and robust. But also tedious, and above a certain level of activity it becomes unmanageable.

2. Calculate gains using the built-in lot tracking syntax of Ledger or Beancount. Beancount's is more robust.
   These may be too limited to calculate gains accurately, eg when there are inter-wallet transfers.

3. Online cryptocurrency tax calculators (Bitcoin.Tax, Cointracker, Coinledger, Koinly, Summ, TokenTax..)
   These let you upload transaction history from all of your cryptocurrency activities.
   Then they analyse this data, identify what happened (you may need to add configuration here), and provide gains reports.
   These apps are convenient and featureful, eg they may understand your country's tax rules; but they also add rather serious privacy risk.
   When they are hacked or infiltrated, potentially your entire past, present and future cryptocurrency activities can be analysed.

4. Offline cryptocurrency tax calculators. These tend to be more private.
   - rotki - AGPL/freemium, featureful, does not support US yet (wallet-based cost tracking, https://github.com/rotki/rotki/issues/2438)
   - RP2 - Apache, does not support US yet (https://github.com/eprbell/rp2/issues/135)
   - BittyTax - AGPL, UK and US variants (can do wallet-based cost tracking by using multiple configs)

So the main focus here is exporting to offline tools.
These require CSV data in a specific format; examples are collected here.
Each record represents an event in a particular cryptocurrency wallet.
There are basic events like deposit, withdrawal, buy, sell, expense, income; and some more specialised event types.
These tools usually provide general gains reports, which you must interpret or adjust according to your country's tax rules.

In the US, universal cost tracking was used through 2024; from tax year 2025, per-wallet cost tracking is required.
The "Safe Harbor" rule allows you to allocate pre-2025 lots to specific wallets (once), which could help optimise taxes in some cases.
