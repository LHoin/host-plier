_hostp_comp()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    local prev=${COMP_WORDS[COMP_CWORD-1]}
    local hostlist=""

    if [[ ${COMP_CWORD} -eq 1 ]] ; then
      COMPREPLY=( $(compgen -W "$(hostp op)" $cur) )
      return 0
    fi

    if [[ ( ${prev} == "open" || ${prev} == "gopen" || ${prev} == "close" || ${prev} == "gclose") && ${COMP_CWORD} -eq 2 ]] ; then
      COMPREPLY=( $(compgen -W "$(hostp hostlist)" $cur) )
      return 0
    fi
}
