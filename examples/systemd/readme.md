# Setup hledger-web with a service file and reverse proxy

## Systemd service file

This setup describes a user service file setup, with hledger-web serving it's web app.

Copy the template service file in this directory to `$HOME/.config/systemd/user`.

Edit your service file and change the settings for your needs.

Start the web app by running `systemctl --user start hledger-web.service`.

If you want your app to start on bootup, then run `systemctl --user enable hledger-web.service`.

## Reverse proxy

### Nginx

The `hledger.nginx` file in this directory has the basics of what you'll need to setup a reverse proxy server.

I would strongly suggest you integrate some kind of security in place to protect your data. How to do that is beyond the scope of
this document.
