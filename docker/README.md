## Instructions

Build slim container with binaries only:
```
./build.sh
```

Build container suitable for development:
```
./build-dev.sh
```

Run hledger-web in the container:
```
./run.sh /path/to/your.journal web
```

Run shell in the container (your files will be in /data):
```
./run.sh /path/to/your.journal bash
```

Run hledger command in the container:
```
./run.sh /path/to/your.journal hledger [ARGS]
```

Script `start.sh` is included inside container and used to start `hledger-web`, there is no need to run it manually.
