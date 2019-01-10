#!/bin/bash
# Completion script for hledger.
# Created using a Makefile and real hledger.

#set -eo pipefail

completeFunction() {
    declare cmd=$1
    declare wordToComplete=$2
    declare precedingWord=$3

    declare subcommand
    for subcommand in "${COMP_WORDS[@]}"; do
	if grep -Fxq "$subcommand" commands.txt; then
	    #declare -a options
	    #readarray -t options <(grep "^$wordToComplete" "$subcommand-options.txt")
	    #COMPREPLY+=( "${options[@]}" )
	    COMPREPLY+=( $(cat "$subcommand-options.txt" | grep "^$wordToComplete") )
	    break
	fi
	subcommand=
    done

    if [[ -z $subcommand ]]; then

	# echo;echo no subcommand

	case $precedingWord in
	    -f|--file|--rules-file)
		# COMPREPLY+=( $(compgen -df | grep "^$wordToComplete") )
		:
		;;
	    *)
		# echo "completing sub commands and general options"
		COMPREPLY+=( $(cat commands.txt generic-options.txt | grep "^$wordToComplete") )
		;;
	esac

    else
	:
	# echo;echo subcommand is $subcommand

	# if grep -Eqv '\b(register|reg|r)\b' <<< "$COMP_LINE"; then
	#     return
	# fi
	# case $precedingWord in
	    # register|reg|r) : ;;
	    # *) return 1 ;;
	# esac

	declare journalFile
	# TODO try to get file from -f --file first
	if [[ -n $HLEDGER_FILE ]]; then
	    journalFile=$HLEDGER_FILE
	else
	    journalFile=~/.hledger.journal
	fi
	COMP_WORDBREAKS=' '
	COMPREPLY+=( $(sed -rn 's/^ +([-_:a-zA-Z0-9]+).*/\1/p' "$journalFile" | grep "^$wordToComplete") )

    fi

}

complete -F completeFunction hledger
