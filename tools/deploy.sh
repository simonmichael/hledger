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

# add latest wiki sidebar links to home page, and push right away so we can keep pulling 
make --no-print-directory site/index.md-push && \

# ensure GHC can handle non-ascii
export LANG=en_US.UTF-8 && \

# ensure latest Shake is built
./Shake.hs \

# update website's generated content (eg manuals) and html
./Shake website \

# print and log to:
) 2>&1 | tee -a deploy.log
