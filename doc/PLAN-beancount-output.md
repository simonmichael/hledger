# Easier export to Beancount and rustledger

Figure out if lot subaccounts are a problem for beancount export.
Avoid --lots ? What about explicit lot subaccounts ?

Some current incompatibilities, seen with c.beancount:

- beancount spurious no position matched errors for acquires
- rustledger won't balance a certain txn
- rustledger doesn't support single-posting transactions

When generating `open` account directives,
we should add the account's lots tag value, if any, as the disposal method.
It should be a method supported by beancount (probably one of FIFO, LIFO, STRICT).
