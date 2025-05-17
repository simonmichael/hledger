# Justfile has replaced this.
MSG="Project scripts are now in Justfile (and Shake.hs). Run 'just' for help."
.PHONY: site
site:
	@echo $(MSG)
%:
	@echo $(MSG)
