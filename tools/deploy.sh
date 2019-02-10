#!/bin/sh
# Update website(s): hledger.org
#
# Called via github webhook (on push to main repo or wiki)
#  /etc/supervisord.conf -> [program:github-post-receive]
#  /etc/github-post-receive.conf
# and via cron (nightly)
#  /etc/crontab
# or manually (make deploy)

(\

# timestamp
echo && date --rfc-3339=seconds && \

# fetch latest code & website - sometimes already done by webhook, not always
git pull && \

# fetch latest wiki content
printf "wiki: " && git -C wiki pull && \

# ensure GHC can handle non-ascii
export LANG=en_US.UTF-8 && \

# ensure latest Shake is built
./Shake.hs && \


# update website
./Shake site/index.md && \
./Shake website \

# print and log to:
) 2>&1 | tee -a deploy.log
