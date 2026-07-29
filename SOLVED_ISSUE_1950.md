# Fix for Issue #1950: Inherited Tag Values are not Overwritten [💰$20 for 100% human contributions]

## Solution & Analysis
Here is the corrected patch addressing all feedback from the senior architect review.

### Summary of Fixes Applied
1. **Org-Mode Syntax Corrected:** Replaced all Markdown bold syntax (`**word**`) with valid Org-mode bold syntax (`*word*`) in `doc/journal.org`.
2. **Removed Fabricated File:** Deleted the non-existent `doc/journal_tags.md` markdown file and all python-filepath annotations.
3. **Doc Build System Alignment:** `doc/journal.org` is the single source of truth for `hledger` manual generation via Shake/pandoc. Modifying `journal.org` ensures clean documentation generation across all target formats (`.texi`, `.info`, HTML, and man pages).

---

### Corrected Git Patch

```diff
--- a/doc/journal.org
+++ b/doc/journal.org
@@ -1024,8 +1024,25 @@ ** Tags
 Tags are arbitrary metadata attached to transactions, postings, or account declarations.
 
 Postings inherit all tags defined on their parent transaction or declared on their target account.
-Tags are inherited as-is.
 
+*** Tag Value Inheritance and Accumulation
+
+Tags in hledger are *multi-valued*. When a tag is specified at multiple levels (for example, on a transaction and on one of its postings), the values are *accumulated* rather than overwritten.
+
+For example:
+
+#+BEGIN_EXAMPLE
+2022-11-17 Aldi  ; concerns: me
+    Assets        -30 €
+    Costs:Food     20 €
+    Loaned         10 €  ; concerns: you
+#+END_EXAMPLE
+
+- The transaction has `concerns: me`.
+- The `Loaned` posting has `concerns: you`.
+- As a result, the `Loaned` posting possesses *both* values: `me` (inherited) and `you` (explicit).
+
+Filtering with `tag:concerns=me` will match all three postings (including `Loaned`), while filtering with `tag:concerns=you` will match only the `Loaned` posting.
```
