# AI policy

The productivity benefits from AI-assisted software development are becoming obvious.
The many potential costs and risks will keep becoming clearer.

Here are some current policies for AI use in the hledger project:

- hledger 1.x has been developed without AI assistance.

- hledger 2.x is developed with careful AI assistance.

- hledger does not use AI at runtime.

- We prefer the more principled/trustworthy/sustainable tools and providers.
  For now that means we prefer Anthropic, Ecosia, local LLMs, and such.

- We monitor and try to limit and optimise our cloud-AI resource usage
  (represented by tokens, cost, etc.)

- We monitor the impact of AI tools on the project and ourselves and make adjustments as needed.

Justification for AI use in this project:

- I needed it to fully design and implement robust automated lot tracking in hledger.
  This is a feature that I have been wanting for years, but it was just too big/intricate to tackle.
  Use of AI tools made it possible. I think it's unlikely hledger would have ever got this feature without them.

- Although similar features exist in other free software (Beancount/Ledger/rustledger/BittyTax/rotki/RP2/..),
  I believe this new implementation provides flexibility currently not available elsewhere -
  private, plain text, and capable of modelling real world lot operations and US pre- and post-2025 booking methods.
  This (I hope) provides value to many.

- I imagine it is overall more efficient in resources and human energy,
  for developers to use AI to develop efficient deterministic software,
  than to have everyone using AI individually to try and do the same tasks less efficiently and less reliably.
  Ie, let's move the AI use upstream as far as possible - use it briefly at design/implementation time,
  not repeatedly at usage time.

- The "bitter lesson" says that general computation always wins over special-purpose
  systems in the end - suggesting that the lifetime and value of specialised tools like hledger will decrease.
  However, there is at least a time lag, and for some time yet there will be a gap in efficiency, cost, reliability,
  and so on.

- We are making mindful limited use of unsustainable technologies for a short time, 
  in preparation for more sustainable versions (local LLMs, ASIC LLMs) coming soon.

- It is a learning experiment that can be discontinued or even rolled back at any time.

If you are a hledger user who objects to any use of AI, for one reason or another:
I can understand. AI is a tool, probably too dangerous for us, but it's here and we're going to have to try to survive it.
The AI-free hledger 1.x still exists, will continue to receive at least regression fixes,
and can be revived or forked at any time if needed.
