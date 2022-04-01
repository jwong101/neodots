# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

#PROFILE_ZSH=1
if [[ -n "${PROFILE_ZSH}" ]]; then
  zmodload zsh/zprof
fi

source "${ZDOTDIR}/plugins/zoxide/zoxide"
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

setopt append_history
setopt share_history
setopt extended_history
setopt histignorespace
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_no_store

setopt autocd
setopt auto_pushd
setopt pushd_ignore_dups
setopt noglobdots
setopt unset
setopt extendedglob
unsetopt beep


typeset -U cdpath CDPATH fpath FPATH manpath MANPATH

source "${ZDOTDIR}/env.sh"
source "${ZDOTDIR}/paths.zsh"
source "${ZDOTDIR}/aliases.sh"
eval "$(pyenv virtualenv-init -)"

export HISTFILE="${XDG_STATE_HOME:-$HOME/.state}/zsh/history"
export HISTSIZE=100000
export SAVEHIST=100000
fpath=(${ZDOTDIR}/{comp,func} "${fpath[@]}")
autoload -Uz "${fpath[2]}"/*(.:t)

export KEYTIMEOUT=5

bindkey -v

bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^[b' vi-backward-blank-word
bindkey -M viins '^[w' vi-forward-blank-word

my-backward-delete-word() {
  local WORDCHARS=${WORDCHARS/\//}
  zle backward-delete-word
}

zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word
bindkey '^[d' kill-word

bindkey -M viins '^U' backward-kill-line
bindkey '^R' history-incremental-search-backward

# bindkey -M viins '\ey' yank-pop
# bindkey -M viins '^P' push-line-or-edit
# bindkey -M viins '^P' yank

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char


# export _ZL_DATA="${XDG_CACHE_HOME}/zsh/zlua"
# source "${ZDOTDIR}/plugins/z.lua/z.lua.plugin.zsh"
# source "${ZDOTDIR}/plugins/czmod/czmod.zsh"
# source "/opt/asdf-vm/lib/asdf.sh"

source "${ZDOTDIR}/bashcomp/_pip"
source "${ZDOTDIR}/bashcomp/pyenv"
source "${ZDOTDIR}/bashcomp/forge"
autoload -Uz _pyenv
_pyenv

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_DEFAULT_OPTS='
--layout=reverse
--inline-info
--height 50%
--multi
--cycle
--bind "ctrl-y:execute:echo -n {} | wl-copy -n"
'
# --color=bg+:#1f2430,bg:#1a1b26,hl:#
export FZF_COMPLETION_TRIGGER=''
bindkey '^T' fzf-completion
bindkey '^I' $fzf_default_completion

fzf-history-widget-accept() {
  fzf-history-widget
  zle accept-line
}
zle -N fzf-history-history-widget-accept
bindkey '^X^R' fzf-history-widget-accept
# source '/usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh'
source "${ZDOTDIR}/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh"
bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down


#local compdump_cache="${XDG_CACHE_HOME}/zsh/zcompdump-${ZSH_VERSION}"

autoload -Uz compinit

if [[ -n "${ZDOTDIR}/.zcompdump"(#qN.mh+24) ]]; then
  compinit -i
  compdump
else
  compinit -C
fi


autoload -Uz bashcompinit && bashcompinit
source "${ZDOTDIR}/bashcomp/_pipx"
source "${ZDOTDIR}/bashcomp/pnpm.zsh"
source "${ZDOTDIR}/bashcomp/kubie"

zstyle ':completion:*' completer _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME}/zsh/zcompcache"

zstyle ':completion:*' menu select
zstyle ':completion:*:messages' format ' %F{orange} -- %d -- %f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}

autoload -Uz bracketed-paste-magic

_urlencode() {
  local length="${#1}"
  for (( i = 0; i < length; i++ )); do
      local c="${1:$i:1}"
      case $c in
        %) printf '%%%02X' "'$c" ;;
        *) printf "%s" "$c" ;;
      esac
  done
}

osc7_cwd() {
  printf '\e]7;file://%s%s\e\\' "$HOSTNAME" "$(_urlencode "$PWD")"
}

autoload -Uz add-zsh-hook
add-zsh-hook chpwd osc7_cwd

autoload -Uz zmv
alias zcp='zmv -C'
alias zln='zmv -L'

export cdpath=(. ~ ~/.config)

zmodload zsh/zpty

pty() {
  zpty pty-"${UID}" "${1+$@}"
  if [[ ! -t 1 ]]; then
      setopt local_traps
      trap '' INT
  fi
  zpty -r pty-"${UID}"
  zpty -d pty-"${UID}"
}

ptyl() {
  pty $@ | less
}

ptyb() {
  pty $@ | bat
}

fh() {
  fc -l 1 | fzf +s --tac | sed 's/ *[0-9]* *//'
}

md() {
  mkdir -p "$1" && cd "$1"
}

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
# export DIRENV_LOG_FORMAT=""

# eval "$(asdf exec direnv hook zsh)"
# direnv () { asdf exec direnv "$@"; }

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

if [[ -n "${PROFILE_ZSH}" ]]; then
  zprof
fi

if [[ -n "${INSIDE_EMACS}" ]]; then
  bindkey -e
fi

# opam configuration
[[ ! -r /home/joshua/.local/share/opam/opam-init/init.zsh ]] || source /home/joshua/.local/share/opam/opam-init/init.zsh  > /dev/null 2> /dev/null
