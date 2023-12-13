---
name:  A bug
about: A weakness in the software, documentation, usability, or project
labels: A BUG
---

Thanks for reporting! Here are some tips (please delete them before submitting):

- Have you checked the hledger manuals, and the right version ?
  Eg for `hledger`: run `hledger help`, or go to
  https://hledger.org/hledger.html and select your version at the top.

- If you're not sure this is a bug, or if some discussion would help,
  contact us on chat or the mail list first:
  https://hledger.org/support.html

- Not required, but any of these can help get issues resolved faster:
  - A small reproducible example
  - The output of hledger --version
  - What platform you're on
  - Links to any related docs that you found

- If you have the access level to set labels, consider adding
  - Any topic labels that seem appropriate
  - Severity and impact labels estimating 

    - How severe is this bug ?

      - severity5: Data loss or privacy/security loss. A user would drop the product.
      - severity4: Crash or bothersome regression or major usability or documentation issue. A user may look for an alternative product.
      - severity3: Installability, packaging or new user experience issue. A potential user would fail to get started.
      - severity2: Minor/moderate usability/doc issue. Easy to avoid or not a big deal.
      - severity1: Cleanup/design/developer issue. Significant only to developers and design-minded users.

    - Who is likely to be impacted by this bug ?

      - impact5: All users.
      - impact4: Most users.
      - impact3: A minority of users.
      - impact2: Only packagers or developers.
      - impact1: Almost no one.

(These are now in the issue tracker as severityN and impactN labels, keep synced.)