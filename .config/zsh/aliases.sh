alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip -color=auto'
alias dmesg='dmesg --color=auto'
alias ls='exa --icons --group-directories-first --git-ignore'
alias l='exa -l --icons --group-directories-first --changed --git --git-ignore --color-scale'
alias ll='exa -l --icons --group-directories-first --changed --git --color-scale'
alias la='exa -a --icons --group-directories-first --git-ignore'
alias lla='exa -l -a --icons --group-directories-first --changed --git --color-scale'
alias pacman='pacman --color=auto'
alias gdb='gdb -q'
alias g='git'
alias mbsync="mbsync -c ${XDG_CONFIG_HOME}/isync/mbsyncrc"
alias jcl='journalctl --user -r'
alias jcls='journalctl -r'
alias sysu='systemctl --user'
alias sys='systemctl'
alias vi='nvim'
alias mutt='neomutt'
alias info='info --vi-keys'
alias mitmproxy="mitmproxy --set confdir=${XDG_CONFIG_HOME}/mitmproxy"
alias mitmweb="mitmweb --set confdir=${XDG_CONFIG_HOME}/mitmproxy"
alias ltrace="ltrace -C -F ${XDG_CONFIG_HOME}/ltrace/ltrace.conf"
alias ict='kitty +icat'

# kubernetes

alias lok='KUBECONFIG=./kubeconfig.yaml kubectl'
alias k='kubectl'
alias kctx='kubectx'
alias kns='kubens'
alias krew='kubectl-krew'
