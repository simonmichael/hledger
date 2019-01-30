#!/bin/sh
cd `git rev-parse --show-toplevel`
docker image build --rm --tag hledger .
