# kyhan's own alias
alias ls='ls --color=auto'
alias ll='ls -alFh'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'

alias resource='source ~/.bashrc'
alias rrrm='rm -rf'
alias cccp='cp -r'
alias ..='cd ..'
alias catbashrc='cat ~/.bashrc'

# set my PS1 for screen/docker or podman/terminal
# if [ S == $(who am i | awk '{print $5}' | cut -d: -f 3 | cut -d. -f 1) ]; then
if ( env | grep TAFFISH_ENV > /dev/null 2>&1 ); then
    if ( env | grep TAFFISH_NAME > /dev/null 2>&1 ); then
	    PS1='($TAFFISH_ENV)[\u@$TAFFISH_NAME] \W\$ '
	else
		PS1='($TAFFISH_ENV)[\u@$HOSTNAME] \W\$ '
	fi
else
    PS1='[\u@(TAFFISH):$HOSTNAME] \W\$ '
fi
