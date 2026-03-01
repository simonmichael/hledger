# AI policy

(WIP)

By now the productivity benefits from "AI"-assisted development are becoming obvious.
And, the many potential costs and risks will keep becoming clearer.

Here are some current policies on AI use in the hledger project:

- hledger 1.x has been developed without AI assistance.

- hledger 2.x is developed with careful AI-assistance.

- hledger does not use AI at runtime.

- We prefer the less unprincipled, less untrustworthy providers. 
  For now that means we prefer Anthropic, Ecosia, and such.

Justification for AI use in this project:

- I needed it to fully design and implement robust automated lot tracking in hledger.
  This is a valuable (I hope) feature, that I have been wanting for years, but it was just too big to tackle.
  Use of AI tools made it possible, and I think it was unlikely to ever happen without them.

- I hope it is overall more efficient in resources and human energy,
  to use AI once to develop efficient deterministic software providing real world value to many,
  than to have everyone using AI to try and do the same tasks less efficiently and less reliably.
  Ie, let's move the AI use upstream as far as possible - use it briefly at design/implementation time,
  not repeatedly at usage time.

- The "bitter lesson" says that general computation always wins over special-purpose
  systems in the end - suggesting that the lifetime and value of specialised tools like hledger will decrease.
  However, there is at least a time lag, and for some time yet there will be a gap in efficiency, cost, reliability,
  and so on.

- We are making mindful limited use of unsustainable technologies for a short time, 
  in preparation for more sustainable versions (local LLMs, ASIC LLMs) coming soon.

- It is a learning experiment that can be discontinued or even rolled back at any time.

For hledger users who object to any use of current AI tools/services:
hledger 1.x still exists, will continue to receive at least regression fixes,
and can be revived or forked at any time if needed.

