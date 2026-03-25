# AI policy

The productivity benefits from AI-assisted software development are becoming obvious.
And the disruptions, costs and risks of AI will keep becoming clearer.

From 2026, in the hledger 2.x series, we are exploring the ethical and effective use of AI to assist hledger development.

If you are a hledger user who objects to any use of AI, for one reason or another: I can understand.
The AI-free hledger 1.x still exists, will continue to receive regression fixes, and can be revived or forked at any time.

## Justification for AI use in this project

- I needed it to fully design and implement robust tax lot tracking in hledger.
  This is a feature that I have been wanting for many years, but it was just too big/intricate to tackle.
  Use of AI tools made it possible. I think it's unlikely hledger would have ever got this feature without them.

- Although lot tracking features exist in other free software (Beancount/Ledger/rustledger/BittyTax/rotki/RP2/..),
  I believe this new implementation provides flexibility currently not available elsewhere
  (privately and precisely modelling real world lot movements as required for tax reporting in the US,
  including pre- and post-2025 US booking methods).  This will (I hope) provide value to many.

- I imagine it is overall more efficient in resources and human energy,
  for developers to use AI to develop efficient deterministic software,
  than to have everyone using AI individually to try and do the same tasks less efficiently and less reliably.
  Ie, let's move the AI use upstream as far as possible - use it briefly at design/implementation time,
  not repeatedly at usage time.

- The "bitter lesson" says that general computation always eventually wins over special-purpose systems -
  suggesting that the lifetime and value of specialised tools like hledger will decrease.
  However, there is at least a time lag, and for some time yet there will be a gap in efficiency, cost, reliability,
  and so on, making this work worthwhile.

- We are making mindful limited use of unsustainable technologies for a short time, 
  in preparation for more sustainable versions (local LLMs, ASIC LLMs) coming soon.

- It is a learning experiment that can be discontinued or even rolled back at any time.

## Rules of engagement

Here are our current policies for AI use in hledger.

- hledger does not use AI at runtime.

- hledger 1.x (2007..2025) was developed without AI assistance.

- hledger 2.x (2026..) is developed with careful AI assistance.

- We aim to increase quality and maintainability, not decrease them.

- The codebase remains human maintainable. At any time we can stop using AI and keep moving forward.

- The human committer is responsible for everything in their commits.

- We want to use only the more principled/trustworthy/sustainable tools and providers.
  So we try to use only Anthropic, Ecosia, local LLMs, and such.

- We try to keep track of, optimise, and limit our AI resource usage (measured by tokens in & out, eg).

- We monitor the impact of AI tools on the project, ourselves, and the planet, and make adjustments when needed.

