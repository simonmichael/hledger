# HLedger Web for Sandstorm

## Build instructions

- Clone the repository to your local machine
- [Install vagrant-spk](https://docs.sandstorm.io/en/latest/vagrant-spk/installation/)
- Navigate your terminal to your local hledger repository folder
- Run `vagrant-spk vm up`
- Run `vagrant-spk dev` to run the app in Sandstorm dev mode
- Visit http://local.sandstorm.io:6080 in your browser to see changes and test
- Press Ctrl-C to exit dev mode
- Run `vagrant-spk pack hledger.spk` to create the Sandstorm package file