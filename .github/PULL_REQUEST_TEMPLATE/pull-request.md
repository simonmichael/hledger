Thanks for your pull request! Your work is appreciated.  

We like to merge as quickly as possible without endangering the project's long term health. 
Much valuable design, quality control, and knowledge sharing happens at merge time, 
ensuring hledger remains a high quality, maintainable product.
Some PRs will require substantial review, discussion, changes, or re-submission.
Some PRs get rejected, but the discussion and exploration can still be a valuable contribution.
We very much want to avoid wasted work, but it can happen. 
All of this is normal. 
Our process is imperfect and evolving, but patience and persistence pays.
Treat it as a golden opportunity to collaborate with experts,
share and receive knowledge, refine your design/documentation/code,
and improve real-world development/communication skills.

Here are some tips to optimise your pull requests and minimise the risk of waste.

A PR should have a clear purpose, documented in its description.
Don't tackle too much at once.
Mention any #ISSUENOs addressed. 

Rebase your PR branch against latest master, especially if it starts to conflict.

Commits should be easy to review.
Ideally each commit is complete, and has a single clear purpose,
documented in the short (and long, if needed) description.
User-visible changes should be described in user-relevant language.
\#ISSUENOs can be mentioned here too when appropriate.
Consider prefixing short descriptions with short component codes (see dev docs -> Issues)
to facilitate history review and changelog/release note production.
All else being equal, fewer, larger commits are preferred.
Rewrite and force-push your commits freely (rebase -i, push -f) to clean them up. 
Usually your commits will become part of hledger's commit history "for all time",
so think about future devs trying to understand them, git bisect, etc.   

hledger follows documentation-driven design. 
All PRs introducing user-visible changes should include corresponding updates to reference docs.
Review can proceed more efficiently once these are provided, and may be delayed otherwise.

"Reference docs" typically means the manual source files, hledger*/hledger*.m4.md.
Updating the rendered manuals is not required, and probably best avoided to reduce conflicts.
Updating other docs such as tutorials, how-tos, examples, or screenshots is not required,  
though it's welcome (may be in a different repo).

Especially for new features, it is in fact highly effective and recommended 
to draft and discuss the new user docs before writing code, 
to minimise wasted work and clarify design and consensus most economically.
This can be done by sharing pastes on IRC, on the mail list, in an issue comment,
or by starting a "WIP" PR with the proposed docs commit.
