tags\
List all the tag names used in the journal. With a TAGREGEX argument,
only tag names matching the regular expression (case insensitive) are shown. 
With QUERY arguments, only transactions matching the query are considered.  

_FLAGS_

There's no direct way to list a tag's values, but there is an indirect way:
--pivot converts a tag's values to accounts, which you can list, like this:

    hledger --pivot SOMETAG accounts --used
