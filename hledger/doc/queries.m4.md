# QUERIES

One of hledger's strengths is being able to quickly report on precise subsets of your data.
Most commands accept an optional query expression, written as arguments after the command name,
to filter the data by date, account name or other criteria.
The syntax is similar to a web search:
one or more space-separated search terms,
quotes to enclose whitespace,
optional prefixes to match specific fields.
Multiple search terms are combined as follows:

All commands except print:
show transactions/postings/accounts which match (or negatively match)

- any of the description terms AND
- any of the account terms AND
- any of the status terms AND
- all the other terms.

The print command:
show transactions which

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

The following kinds of search terms can be used:

**`REGEX`**
: match account names by this regular expression

**`acct:REGEX`**
: same as above

**`amt:N, amt:<N, amt:<=N, amt:>N, amt:>=N`**
: match postings with a single-commodity amount that is equal to, less
than, or greater than N.  (Multi-commodity amounts are not tested, and
will always match.)  The comparison has two modes: if N is preceded by
a + or - sign (or is 0), the two signed numbers are
compared. Otherwise, the absolute magnitudes are compared, ignoring
sign.

**`code:REGEX`**
: match by transaction code (eg check number)

**`cur:REGEX`**
: match postings or transactions including any amounts whose
currency/commodity symbol is fully matched by REGEX. (For a partial
match, use `.*REGEX.*`). Note, to match characters which are
regex-significant, like the dollar sign (`$`), you need to prepend `\`.
And when using the command line you need to add one more level of
quoting to hide it from the shell, so eg do: `hledger print cur:'\$'`
or `hledger print cur:\\$`.

**`desc:REGEX`**
: match transaction descriptions

**`date:PERIODEXPR`**
: match dates within the specified period.
PERIODEXPR is a [period expression](#period-expressions) (with no report interval).
Examples: `date:2016`, `date:thismonth`, `date:2000/2/1-2/15`, `date:lastweek-`.
If the `--date2` command line flag is present, this matches [secondary dates](manual.html#secondary-dates) instead.

**`date2:PERIODEXPR`**
: match secondary dates within the specified period.

**`depth:N`**
: match (or display, depending on command) accounts at or above this depth

**`real:, real:0`**
: match real or virtual postings respectively

**`status:, status:!, status:*`**
: match unmarked, pending, or cleared transactions respectively

**`tag:REGEX[=REGEX]`**
: match by tag name, and optionally also by tag value.  Note a
tag: query is considered to match a transaction if it matches any of
the postings.  Also remember that postings inherit the tags of their
parent transaction.

**`not:`**
: before any of the above negates the match.

**`inacct:ACCTNAME`**
: a special term used automatically when you click an account name in hledger-web, 
specifying the account register we are currently in
(selects the transactions of that account and how to show them, can be filtered further with `acct` etc).
Not supported elsewhere in hledger.

Some of these can also be expressed as command-line options (eg `depth:2` is equivalent to `--depth 2`).
Generally you can mix options and query arguments, and the resulting query will be their intersection
(perhaps excluding the `-p/--period` option).
