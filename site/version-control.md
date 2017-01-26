# Track changes with version control

You don't need to do this, but it's a nice way to keep track of changes to your data.  

## git

Start tracking changes:\
`git init && git add 2017.journal && git commit 2017.journal -m "first commit"`

View uncommitted changes: `git status`, `git diff` 

Commit changes: `git commit 2017.journal -m "updates"`

View past commits: `git log`

## darcs

`darcs init && darcs add 2017.journal && darcs record 2017.journal -m "first commit"`

`darcs whatsnew`, `darcs diff`

`darcs record 2017.journal -m "updates"`

`darcs log`

## etc.