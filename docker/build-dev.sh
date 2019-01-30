#!/bin/sh
cd `git rev-parse --show-toplevel`
docker image build --tag hledger --target dev .
