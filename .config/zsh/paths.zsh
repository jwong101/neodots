typeset -U path PATH
export PATH="${GHCUP_INSTALL_BASE_PREFIX:-$HOME/.local/share}/.ghcup/bin:${PATH}"
export PATH="${GOPATH:-$HOME/.local/share/go}/bin:${PATH}"
export PATH="${CARGO_HOME:-$HOME/.local/share/cargo}/bin:${PATH}"
export PATH="${KREW_ROOT:-$HOME/.local/share/krew}/bin:${PATH}"
#export PATH="${DENO_INSTALL_ROOT:-$HOME/.local/share/deno}/bin:${PATH}"
# export PATH="/opt/asdf-vm/bin:${PATH}"
export path=("${PYENV_ROOT:-$HOME/.local/share/pyenv}"/{bin,shims} "${path[@]}")
#export PATH="${HOME}/tools/depot_tools:${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"
