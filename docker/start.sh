#!/bin/bash

echo "host: ${HLEDGER_HOST:=0.0.0.0}"
echo "port: ${HLEDGER_PORT:=5000}"
echo "base url: ${HLEDGER_BASE_URL:="http://localhost:$HLEDGER_PORT"}"
echo "file url: ${HLEDGER_FILE_URL:=}"
echo "input file: ${HLEDGER_JOURNAL_FILE:=/data/hledger.journal}"
echo "debug level: ${HLEDGER_DEBUG:=1}"
echo "rules file: ${HLEDGER_RULES_FILE:=/data/hledger.rules}"

exec hledger-web \
     --server \
     --host=$HLEDGER_HOST \
     --port=$HLEDGER_PORT \
     --file "$HLEDGER_JOURNAL_FILE" \
     --debug=$HLEDGER_DEBUG \
     --base-url=$HLEDGER_BASE_URL \
     --file-url=$HLEDGER_FILE_URL \
     --rules-file="$HLEDGER_RULES_FILE"
