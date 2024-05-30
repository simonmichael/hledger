[tldr-pages](https://tldr.sh) provides minimal, example-focussed doc pages for command line tools and their subcommands.

tldr pages are short but high value, and a great counterbalance to PTA's verbose docs.
So they are worth prioritising and maintaining.
gutjuri made the first hledger tldr page in 2022, and sm added more in 2024.

This directory has local copies of all [hledger-related tldr pages](https://github.com/search?q=repo%3Atldr-pages%2Ftldr%20hledger&type=code)
The tldr-pages repo has the master copies, so they should be copied here periodically (eg before release).
These docs are crafted first to suit tldr and its style rules, but we'll reuse them where we can.
Eg they are now embedded in the hledger tools and accessible with --tldr.

<https://tldr.inbrowser.app> is an online (& offline) tldr viewer,
where you can search for [hledger command examples](https://tldr.inbrowser.app/search?query=hledger+).

You can also make a web browser keyword for it ([firefox & chrome][1]; [safari][2]),
like `tldr CMD` = `https://tldr.inbrowser.app/search?query=CMD`.
Then in your browser's address bar you can type
`tldr`, `tldr hledger`, `tldr hledger-web`, `tldr hledger-balancesheet`, etc.

In the search field's gear icon you can configure preferred languages.
If you speak a language other than english, please consider making a few [translations](https://github.com/tldr-pages/tldr/blob/main/CONTRIBUTING.md#translations)!
It is relatively easy. You can do it entirely in your web browser if you have a Github account:

1. Starting with eg https://tldr.inbrowser.app/pages/common/hledger
2. click "Find this page on GitHub"
3. click the "Copy raw file" icon (two squares)
4. navigate to eg https://github.com/tldr-pages/tldr/tree/main/pages.de/common
5. click "Add file" > "Create new file"
6. reuse the original file name, paste the original content
7. translate the content
8. commit
9. send a pull request

Here is the [tldr translations status](https://lukwebsforge.github.io/tldri18n).


[1]: https://karl-voit.at/browser-keywords
[2]: http://safarikeywordsearch.aurlien.net
