#!/bin/sh
# Update website(s): hledger.org
#
# Called periodically by cron (eg on the hour)
#  /etc/crontab
# and via github webhook (on push to main repo or wiki)
#  /etc/supervisord.conf -> [program:github-post-receive]
#  /etc/github-post-receive.conf

(\

# timestamp
echo && date --rfc-3339=seconds && \

# fetch latest code & website - should be already done by webhook
# git pull && \

# fetch latest wiki content
git -C wiki pull && \

# add latest wiki sidebar links to home page
make site/index.md && \

# ensure haskell can handle non-ascii
export LANG=en_US.UTF-8 && \

# ensure latest Shake is built
./Shake.hs \

# update website's generated content (eg manuals) and html
./Shake website \

# print and log to:
) 2>&1 | tee -a deploy.log
