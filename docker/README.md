## Build instructions

Build slim container with binaries only
```
docker image build --rm --tag hledger .
```

Build container suitable for development
```
docker image build --tag hledger --target dev .
```
