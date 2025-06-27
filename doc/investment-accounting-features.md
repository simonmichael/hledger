# Investment Accounting Feature Ladder

<div class=pagetoc>

<!-- toc -->
</div>

When trying to understand or build accounting software for investment, 
for example in the context of Plain Text Accounting, 
things get complicated rather fast. 
The following ladder can be helpful when evaluating or building
investment-related features in accounting software.

(hledger users: this is not documentation for hledger; 
if you're looking for that, try [Cookbook > Investing and trading](doc.md#investing-and-trading).)

Each rung of the ladder adds a new concept or feature, describing:

- one small increment to user-visible functionality
- any new accounting/investing concepts needed
- any new simplifying assumptions and scope
- any new changes known to be needed in internal capabilities

This is a start, suggestions welcome in #hledger or #plaintextaccounting matrix chats.

## Assets
Track asset transactions and balances.
Initial assumptions/constraints: 
assets are cash (in a single base currency) or simple investment commodities.
(In this doc we'll call those "cash" and "commodities" respectively.)

## Asset costs
Track purchase costs, in the base currency, of assets. An asset balance can have more than one cost. Allow reporting total cost of assets.

## Asset values
Track fluctuating market prices for individual commodities. Allow reporting current market value of asset balances.

## Report unrealised gains
Allow reporting the unrealised gain or loss of asset balances.
Unrealised gains are the increase (gain) or decrease (loss) of the value of assets you are holding, since you acquired them. The difference between current value and your original cost. 

## Report realised gains
...
