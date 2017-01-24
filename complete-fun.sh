_hostp_comp()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    local prev=${COMP_WORDS[COMP_CWORD-1]}
    local hostlist=""
    IFS=$'\n'

    if [[ ${COMP_CWORD} -eq 1 ]] ; then
      COMPREPLY=( $(compgen -W "$(hostp op)" $cur) )
      return 0
    fi

    if [[ ( ${prev} == "open" || ${prev} == "close" || ${prev} == "remove") && ${COMP_CWORD} -eq 2 ]] ; then
      COMPREPLY=( $(compgen -W "$(hostp hostlist)" $cur) )
      return 0
    fi

    if [[ ( ${prev} == "gopen" || ${prev} == "gclose" ) && ${COMP_CWORD} -eq 2 ]] ; then
      COMPREPLY=( $(compgen -W "$(hostp grouplist)" $cur) )
      return 0
    fi

    if [[ ${COMP_CWORD} -eq 3 ]] ; then
      local op=${COMP_WORDS[1]} 
      local hostname=${COMP_WORDS[2]}
      if [[ ( ${op} == "open" || ${op} == "remove" ) ]] ; then
        COMPREPLY=( $(compgen -W "$(hostp iplist $hostname)" $cur) )
        return 0
      fi
    fi
}
complete -F _hostp_comp hostp
