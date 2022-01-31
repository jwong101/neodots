export CUDA_CACHE_PATH="$HOME/.config/nv"
source "$ZDOTDIR/paths.zsh"
if [[ -z "${DISPLAY}" ]] && [[ -z "${WAYLAND_DISPLAY}" ]] && [[ "${TTY}" = "/dev/tty1" ]]; then
  exec "${HOME}/.local/bin/startw"
fi

