[tldr-pages](https://tldr.sh) provides minimal, example-focussed doc pages for command line tools and their subcommands.

tldr pages are short but high value, and a great counterbalance to PTA's verbose docs.
So they are worth prioritising and maintaining.
gutjuri made the first hledger tldr page in 2022, and sm added more in 2024.

This directory has convenient symlinks for accessing the
[hledger-related tldr pages](https://github.com/search?q=repo%3Atldr-pages%2Ftldr%20hledger&type=code)
(if tldr is cloned as a sibling to the hledger project directory).

The tldr pages are crafted first to suit tldr and its style rules,
but we should try to reuse them elsewhere when it's practical.

If you speak a language other than english, please consider making a few translations!
It is relatively easy:
<https://github.com/tldr/tldr/blob/main/CONTRIBUTING.md#translations>.
Here is where to check [translations status](https://lukwebsforge.github.io/tldri18n).

<https://tldr.inbrowser.app> is an online (& offline) tldr viewer,
where you can search for [hledger command examples](https://tldr.inbrowser.app/search?query=hledger+).

You can make a web browser keyword for it ([firefox & chrome][1]; [safari][2]),
like `tldr CMD` = `https://tldr.inbrowser.app/search?query=CMD`.
Then in your browser's address bar you can type
`tldr`, `tldr hledger`, `tldr hledger-web`, `tldr hledger-balancesheet`, etc.

[1]: https://karl-voit.at/browser-keywords
[2]: http://safarikeywordsearch.aurlien.net
