# makefile self-documentation
# http://www.cmcrossroads.com/print/article/self-documenting-makefiles

help:
	@echo $(if $(need-help),,Type \'make$(dash-f) help\' to get help)

need-help := $(filter help,$(MAKECMDGOALS))

define def-help
	$(if $(need-help),$(warning $1 --$2))
endef

# define print-lines
# 	@echo $1
# endef
#	$(if $(true),$(printf $1),$(printf '\n'$1))

define def-help-section
	$(if $(need-help),$(warning --------------------$1--------------------$2))
endef

define last-element
	$(word $(words $1),$1)
endef

this-makefile := $(call last-element,$(MAKEFILE_LIST))
other-makefiles := $(filter-out $(this-makefile),$(MAKEFILE_LIST))
parent-makefile := $(call last-element,$(other-makefiles))

dash-f := $(if $(filter-out Makefile makefile GNUmakefile, $(parent-makefile)), -f $(parent-makefile))

