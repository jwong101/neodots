{
  local zcompdump="${ZDOTDIR:-$HOME/.config/zsh}/.zcompdump"
  if [[ -s "${zcompdump}" && (! -s "${zcompdump}.zwc" || "${zcompdump}" -nt "${zcompdump}.zwc") ]]; then
    zcompile "${zcompdump}"
  fi
} &!
