Additional notes about the hledger project's AI usage, complementing ../AI.md.

Related scripts are in ../../Justfile: run `just ai-help`

Files in this directory. The parenthesised ones are not committed.

- ccusage.rules       - for reading `ccusage` output to generate ccusage.journal.
- (ccusage.journal)   - a snapshot of local claude code usage (from ccusage).
- ai.rules            - for importing from ccusage.journal's monthly areg report to ai.journal.
- (aicommits.csv)     - a snapshot of AI usage info from the hledger repo's commit messages.
- aicommits.csv.rules - for importing from aicommits.csv to ai.journal.
- ai.journal          - the project's permanent log of estimated AI usage.

