# Helpers for self-documenting makefiles.
# Based on http://www.cmcrossroads.com/print/article/self-documenting-makefiles
#
# Standard usage:
#
# TARGET: PREREQUISITES $(call def-help,TARGET,HELP)
# 	ACTIONS
#
# or on multiple lines:
#
# TARGET: PREREQUISITES \
# 	$(call def-help,TARGET,\
# 	HELP\
# 	)
# 	ACTIONS
#
# Also:
#
# $(call def-help-heading,TITLE,HELP)     -- show a section heading
#
# $(call def-help-subheading,TITLE,HELP)  -- show a subsection heading
#
# $(call def-help-hide,TARGET,HELP)       -- temporarily suppress the help)
# $(call def-help-heading-hide,TITLE,HELP)
# $(call def-help-subheading-hide,TITLE,HELP)
#
# HELP is one or more lines, or can be blank.
# Certain characters are not allowed, comma in particular.
# You may want to avoid ' also as it breaks emacs font-lock.

# if the make targets include "help" or there are no targets, show help
need-help := $(if $(MAKECMDGOALS),$(filter help,$(MAKECMDGOALS)),true)

help:
	@echo $(if $(need-help),,Type \'make$(dash-f) help\' to get help)

# show a make target's help when help has been requested
define def-help
	$(if $(need-help), $(warning  make $1 --$2))
endef

# show a section heading when help has been requested
define def-help-heading
	$(if $(need-help),$(warning $1))
endef

# show a subsection heading when help has been requested
define def-help-subheading
	$(if $(need-help),$(warning );$(warning $1))
endef

# no-ops, for hiding help without removing it entirely
define def-help-hide
endef
define def-help-heading-hide
endef
define def-help-subheading-hide
endef

# utilities

# define print-lines
# 	@echo $1
# endef
#	$(if $(true),$(printf $1),$(printf '\n'$1))

define last-element
	$(word $(words $1),$1)
endef

this-makefile := $(call last-element,$(MAKEFILE_LIST))
other-makefiles := $(filter-out $(this-makefile),$(MAKEFILE_LIST))
parent-makefile := $(call last-element,$(other-makefiles))
dash-f := $(if $(filter-out Makefile makefile GNUmakefile, $(parent-makefile)), -f $(parent-makefile))
