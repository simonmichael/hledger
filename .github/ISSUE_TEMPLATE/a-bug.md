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
  - Appropriate topic labels, if you have the access level to set those
    (If not, feel free to write them in the issue title, eg as a prefix.)
  - Estimates from 1 to 5 of

    - How serious is this bug ?

      - 5: Data loss or privacy/security loss. A user would drop the product.
      - 4: Crash or bothersome regression or major usability or documentation issue. A user may look for an alternative product.
      - 3: Installability, packaging or new user experience issue. A potential user would fail to get started.
      - 2: Minor usability or documentation issue. Noticeable but easy to avoid / not a big deal.
      - 1: Cleanup/design/developer issue. Significant only to developers and design-minded users.

    - Who is likely to be affected by this bug ?

      - 5: All users.
      - 4: Most users.
      - 3: A minority of users.
      - 2: Only packagers or developers.
      - 1: Almost no one.

