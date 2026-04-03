# AI policy

Last updated: 2026-04-03

The productivity benefits from AI-assisted software development are becoming obvious.
And the disruptions, costs and risks of AI will keep becoming clearer.

From 2026, in the hledger 2.x series, we are exploring ethical and effective use of AI to assist hledger development.

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

- PRs require human review; if they are hard to review, they don't get merged, as always.

- We want to use only the more principled/trustworthy/sustainable tools and providers.
  Currently this means we try to use only Anthropic, Ecosia, local LLMs, and such.
  We'll review/update the list periodically.

- Since writing this policy, I applied for and received Anthropic's FOSS 6-month Claude Max credit (currently worth $1200),
  and activated it from 2026-04-01.
  This is helping recover the costs I incurred during initial experimentation.

- We try to keep track of, optimise, and limit our AI resource usage (measured by tokens in & out, eg).

- We monitor the impact of AI tools on the project, ourselves, and the planet, and make adjustments when needed.


## More discussion / thoughts

### Can engaging with the AI phenomenon be ethical ?

- Choosing to avoid AI use entirely will become increasingly hard if not impossible.
But it's one possible stance, requiring Amish- or Stallman-like discipline.
(FSF is working on an AI policy, which will be interesting to read.)
My gut feeling is that absolute non-engagement isn't the right course for most of us.
It's possible that could change.

- It is here and will continue to unfold and impact us all for at least the next N years.
It shouldn't be left only to the boosters and zealots.

### Generating code with AI violates copyright of the training sources ?

- First, I'll note that the topic of copyright comes up only when using AI to generate code (docs, content..)
that is to be published. Much of the discussion here focusses on generated code, but let's keep in mind
that is just one of many ways AI can be used. Allowing AI use but writing all code by hand is a possible policy.
- Both in manual and automated coding, there's much grey area between clear plagiarism/theft, and fair use/independent invention.
- At some point, code is like science or mathematics: not ownable, belonging to all.
- "Copyright" is not a law of nature, not a consistent human law around the world, and not consistently applied,
eg to individuals and to large corporations. It's a mechanism, that was invented to achieve certain goals in a certain context.
The deeper goals we really care about are fairness, rule of law, innovation, stability, healthy society, etc.

### AI-generated code has unclear copyright / license ?

sm: I'm assuming it's equivalent to code written by the human committer until we hear otherwise.
I don't see how anything else is practical at present.

gecko: several explicit court rulings in the USA have clearly outlined that AI generated code is not human work, it's machine generated, and therefore no copyright exists for it.
Please see the official website of US congress for more explanation about their legal basis for this (below).
Only in some cases will copyright for AI code be valid.
In March 2025, the U.S. Court of Appeals for the D.C. Circuit affirmed the district court's decision in Thaler v. Perlmutter, holding that the Copyright Act "requires all eligible work to be authored in the first instance by a human being."
But again Europe has different laws around this too, I am unfamiliar with their laws.
It would be cool if we could simply say the human committer is the author but legally that's invalid and wrong, and if fully AI generated code is added to hledger, it's possible governments will decide this can't be copyrighted because it's not human work.
In this case those changes can't be licensed under the GPL or anything too.
This creates a situation where hledger may have large amounts of unlicensed code.

<https://www.congress.gov/crs-product/LSB10922>:

> U.S. courts to date have not recognized copyright in works that lack a human author—including works created autonomously by AI systems.

> "human authorship is an essential part of a valid copyright claim"

> the AI Guidance states that "what matters is the extent to which the human had creative control over the work's expression."

sm: I feel like the lots branch is essentially human-authored, using powerful tools. 
Similar in principle to how the compiled machine code was human-authored, using a compiler to generate it. 
Of course it's a spectrum, and we'll find out what the various courts decide.


Re "generated code seems like a potential time bomb": because of copyright/licensing reasons ?
I think hledger has less to lose there than a large business; we are a GPLv3+ project,
which means in practice nothing depends on us except similar projects.
Worst case, we roll back to hledger 1.x and rewrite things by hand.

### AI-generated code should be explicitly tagged ?

- That's a policy that might make sense in some cases. (I have annotated the merge of the recent lots branch this way, eg.)
But there's a spectrum of AI involvement which would complicate this,
and in the most general case I don't think it's enforceable or scalable.
I think it's more practical to keep associating authorship and responsibility with the human committer.

### AI's environmental impact ?

- It's bad of course, probably worse than we can see, and will remain so until we require it not to be.
It will continue to become more efficient and less costly in resources, possibly more quickly than we might expect.
Smaller AIs running on local devices powered by solar energy, eg.
Offsetting costly AI use with compensating actions could be helpful. (As eg Ecosia does.)
- We live in a time of collapse. Are current AI use and trends sustainable ?
Will widespread reliable energy, resources, infrastructure, stability, etc. continue to allow it ?
No, at least not in the ways we're used to. But AI is going to remain "cheap" and widely used for a while yet.
- Are the true costs of AI (capitalism+AI, really) being hidden from users ? Of course, as usual.
We should strive for more complete accounting, transparency, and truthful discourse
that includes all impacts on society and the planet.

### After 18 years of development, what was the sudden shortcoming?
Was it a shortage of time/labor or a technical problem that humans couldn't solve alone?

- Lots tracking in hledger was unimplemented for many years,
because the cost of designing and building it outweighed the need and the available resources
(part-time FOSS developer + occasional contributors). Lately,
    - The need has intensified. US tax reporting has become more strict and more complex,
    requiring the aid of software tools, and in the cryptocurrency era most of those are a security risk.
    - The latest AI tools (opus 4.6, claude code..) have dramatically lowered the cost.
    - As a lifelong programmer, once you have experienced the labour saving of a good AI setup,
    it becomes very hard to keep forcing your mind to jump through those hoops which are better suited to a machine.
    It feels like a waste of human spirit and energy.
    - Given the times, this was a good project to motivate exploring the new tech and surrounding issues,
    and how to use it or to respond to others using it.

## Measuring usage

Some notes on tracking AI usage.

### platform.claude.com/usage

For the lots work in the 2.0pre1 release,
here's an estimate of my (SM's) claude token use (in+out) and cost,
from <https://platform.claude.com/usage>.

- 2026-01: 133 Mt,   $85
- 2026-02: 598 Mt,  $551
- 2026-03: 299 Mt,  $256
- Total:    ~1 Gt, ~$900

Estimated human dev time: ~150h, market value ~$10k-30k

This report works only for API usage; it shows nothing after switching to subscription billing.

### ccusage

[ccusage](https://ccusage.com) shows data from the claude code chat logs on your machine,
I believe regardless of how they were billed (API or subscription).
I'm not sure how durable the data is.
Probably when old chat logs get cleaned up or deleted, ccusage no longer shows data for those.

`just h ccusage` shows some related report scripts in the Justfile.
At the start of april, on my machine they showed the following.
I don't know why the numbers are so much lower than the ones from platform.claude.com.

```
$ j ccusage-journal
$ j ccusage-bal
Balance changes in 2026-01-01..2026-04-30, valued at period ends:

           || claude 
===========++========
 Commodity ||     Mt 
 Jan       ||   10.7 
 Feb       ||   67.1 
 Mar       ||  432.8 
 Apr       ||   11.4 
   Total   ||  521.9 
 Average   ||  130.5 
```

## Related

Discussions
- <https://fosstodon.org/@happyborg/116312844334306281> 

Policies
- <https://docs.fedoraproject.org/en-US/council/policy/ai-contribution-policy/>

Law
- <https://www.congress.gov/crs-product/LSB10922>

Practitioners
- Donald Knuth: <https://cs.stanford.edu/~knuth/papers/claude-cycles.pdf>
- Kent Beck: <https://tidyfirst.substack.com/t/genies>,\
  <https://www.youtube.com/watch?v=5htJ2ML7BKU> 
- Martin Fowler: <https://martinfowler.com/fragments/2026-04-02.html>

Research
- [From Technical Debt to Cognitive and Intent Debt: Rethinking Software Health in the Age of AI](https://arxiv.org/pdf/2603.22106)
