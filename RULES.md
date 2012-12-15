# hledger conversion rules v2 (draft)

When hledger reads CSV files, it looks for conversion rules that help
it transform the CSV records into meaningful journal entries. These
usually come from a file, named like the csv file with a `.rules`
suffix added.

The version 2 rules format aims to be more powerful and more
understandable than version 1. This design document describes the
proposed rules and syntax for discussion and refinement.

## Rules syntax

Several kinds of rule may be used. In order of increasing complexity:

1. A **FIELD LIST** is an easy way to name CSV fields, and also to
   assign them to entry fields. It is a comma-separated list of names
   on one line, as is often found at the start of a CSV file.  When
   standard journal entry field names are used (`date`, `date2`,
   `status`, `code`, `description`, `account`, `account2`, `amount`,
   `comment`, `tag`), the csv values in that position are used for
   those entry fields. Eg:

        date, desc, amount, notes, some other field

2. **FIELD ASSIGNMENTS** are a slightly more flexible way to define
   entry fields. They consist of an entry field name, then a colon
   (optional), then a string containing 0 or more CSV field
   references. Eg, here the entry description is built from three CSV
   fields:

        description: %desc (%notes) %"some other field"

3. **ENTRY TEMPLATES** give most flexibility, defining the entire
   journal entry at once. They look like a standard journal entry, but
   with CSV field references instead of values, beginning with `%date`
   on the first line. Eg, this builds an entry with an extra virtual
   posting:

        %date %description
          %account                   %amount
          %default-account
          (some special account)     %amount

4. **CONDITIONAL BLOCKS** enable field assignments or an entry
   template, when a CSV field matches a certain pattern. They consist
   of the word `if` (optional), then a CSV field name, then either `~`
   (for case-insensitive infix regular expression matching) or `=`
   (for case-insensitive exact matching), then the regular expression
   or value to match, then one or more field assignments or one entry
   template, and finally a blank line. Eg, here if description
   contains "groc" then the entry's account and comment are set as
   shown:

        description ~ groc
        account: expenses:groceries
        comment: household 

5. **DIRECTIVES** influence the overall conversion process. They are:

        skip-lines       1                 # skips this number of CSV header lines
        date-format      %d/%m/%y          # parses the CSV date field in this way
        default-currency $                 # adds this currency symbol to amounts without one
        default-account  assets:checking   # uses this value as the default for account2

## Other rules syntax features:

- **COMMENTS** beginning with `#` or `;` are ignored, as are (except
  as noted above) blank lines.

- **FIELD NAMES** are either a word containing no whitespace and no
  comment character (`;` or `#`), or a phrase enclosed by double quotes.

- **FIELD REFERENCES** look like `%name` or `%1`. They are replaced by
  the content of the CSV field with that name or position. A reference
  to a field containing an amount may be written `%-name`, in which
  case the amount's sign will be reversed.

