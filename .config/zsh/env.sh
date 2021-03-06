export EDITOR=nvim
export VISUAL=nvim
export LESS='-RF --use-color -Dd+r$Du+b'
export BROWSER=firefox-nightly
export PAGER=less
export MANPAGER='nvim +Man!'
export MANWIDTH=999

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export LESSHISTFILE="${XDG_DATA_HOME}/lesshst"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GPG_TTY="$TTY"
unset SSH_AGENT_PID

if [[ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]]; then
	export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

export SEMGREP_SEND_METRICS=off
export BOTO_CONFIG="${XDG_CONFIG_HOME}/gsutil/config"
export STEPPATH="${XDG_CONFIG_HOME}/step"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export TF_CLI_CONFIG_FILE="${XDG_CONFIG_HOME}/terraform/config.tfrc"
export KREW_ROOT="${XDG_DATA_HOME}/krew"
export K9S_CONFIG="${XDG_CONFIG_HOME}/k9s"
export AWS_SHARED_CREDENTIALS_FILE="${XDG_CONFIG_HOME}/aws/credentials"
export AWS_CONFIG_FILE="${XDG_CONFIG_HOME}/aws/config"

# postgres
export PSQLRC="${XDG_CONFIG_HOME}/pg/psqlrc"
export PSQL_HISTORY="${XDG_STATE_HOME}/psql_history"
export PGPASSFILE="${XDG_CONFIG_HOME}/pg/pgpass"
export PGSERVICEFILE="${XDG_CONFIG_HOME}/pg/pg_service.conf"

# redis
export REDISCLI_HISTFILE="${XDG_STATE_HOME}/redis/rediscli_history"
export REDISCLI_RCFILE="${XDG_CONFIG_HOME}/redis/redisclirc"

export SQLITE_HISTORY="${XDG_STATE_HOME}/sqlite/sqlite_history"
export MYSQL_HISTFILE="${XDG_STATE_HOME}/mysql/mysql_history"

export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export GDBHISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/gdb/gdb_history"
# export ASDF_DATA_DIR="${XDG_DATA_HOME}/asdf"
# export ASDF_DIR=/opt/asdf-vm

# emacs
export LSP_USE_PLISTS=1

# python
export PIPX_HOME="${XDG_DATA_HOME}/pipx"
export PYTHONSTARTUP="${XDG_CONFIG_HOME}/python/startup.py" # try polluting $HOME now
export PYENV_ROOT="${XDG_DATA_HOME}/pyenv"
export PYENV_SHELL='zsh'
export PYENV_VIRTUALENV_CACHE_PATH="${XDG_CACHE_HOME}/pyenv-virtualenv"
export IPYTHONDIR="${XDG_CONFIG_HOME}/jupyter"
export JUPYTER_CONFIG_DIR="${IPYTHONDIR}"
export JUPYTERLAB_DIR="${XDG_CONFIG_HOME}/jupyter/lab"
export JUPYTERLAB_SETTINGS_DIR="${JUPYTERLAB_DIR}/user-settings"
export JUPYTERLAB_WORKSPACES_DIR="${XDG_DATA_HOME}/jupyter/lab/workspaces"
export WORKON_HOME="${XDG_DATA_HOME}/virtualenvs"
export DOT_SAGE="${XDG_CONFIG_HOME}/sage"
command pyenv rehash 2>/dev/null
#eval "$(pyenv init -)"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="${XDG_CONFIG_HOME}/java"

# ocaml
export OPAMROOT="${XDG_DATA_HOME}/opam"
export DUNE_CACHE='enabled'

# javascript
export NODE_REPL_HISTORY="${XDG_STATE_HOME}/node/node_repl_history"
export COREPACK_HOME="${XDG_DATA_HOME}/corepack"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export DENO_INSTALL_ROOT="${HOME}/.local/bin"
export DENO_DIR="${XDG_CACHE_HOME}/deno"

# rust
export RUSTUP_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/rustup"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"

# haskell
export GHCUP_INSTALL_BASE_PREFIX="${XDG_DATA_HOME:-$HOME/.local/share/}"
export CABAL_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/cabal/config"
export CABAL_DIR="${XDG_CACHE_HOME}/cabal"
export STACK_ROOT="${XDG_DATA_HOME}/stack"

# lua
# export LUA_PATH='/usr/share/lua/5.1/?.lua;/usr/share/lua/5.1/?/init.lua;/usr/lib/lua/5.1/?.lua;/usr/lib/lua/5.1/?/init.lua;./?.lua;./?/init.lua;/home/joshua/.luarocks/share/lua/5.1/?.lua;/home/joshua/.luarocks/share/lua/5.1/?/init.lua'
# export LUA_CPATH='/usr/lib/lua/5.1/?.so;/usr/lib/lua/5.1/loadall.so;./?.so;/home/joshua/.luarocks/lib/lua/5.1/?.so'

export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export FFMPEG_DATA_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/ffmpeg"
export WINEPREFIX="${XDG_DATA_HOME}/wineprefixes/default"
export UNCRUSTIFY_CONFIG="${XDG_CONFIG_HOME}/uncrustify/uncrustify.cfg"

export NOTMUCH_CONFIG="${XDG_CONFIG_HOME}/notmuch/notmuchrc"
export QUEUEDIR="${XDG_DATA_HOME}/msmtpqueue"

# readline
export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"
export RLWRAP_HOME="${XDG_DATA_HOME}/rlwrap"

# zoom
export SSB_HOME="${XDG_DATA_HOME}/zoom"

export MPD_HOST="${XDG_RUNTIME_DIR}/mpd/socket"
export MPD_PORT=6600

export LS_COLORS="ln=0;38;2;255;106;193:ex=1;38;2;224;108;117:*~=0;38;2;102;102;102:bd=0;38;2;86;182;194;48;2;51;51;51:tw=0:or=0;38;2;0;0;0;48;2;224;108;117:pi=0;38;2;0;0;0;48;2;97;175;239:st=0:di=0;38;2;97;175;239:cd=0;38;2;255;106;193;48;2;51;51;51:so=0;38;2;0;0;0;48;2;255;106;193:ow=0:fi=0:no=0:mi=0;38;2;0;0;0;48;2;224;108;117:*.r=0;38;2;152;195;121:*.a=1;38;2;224;108;117:*.m=0;38;2;152;195;121:*.o=0;38;2;102;102;102:*.d=0;38;2;152;195;121:*.z=4;38;2;86;182;194:*.t=0;38;2;152;195;121:*.p=0;38;2;152;195;121:*.h=0;38;2;152;195;121:*.c=0;38;2;152;195;121:*.hs=0;38;2;152;195;121:*.as=0;38;2;152;195;121:*.ll=0;38;2;152;195;121:*.ts=0;38;2;152;195;121:*.ko=1;38;2;224;108;117:*.js=0;38;2;152;195;121:*.7z=4;38;2;86;182;194:*.so=1;38;2;224;108;117:*.rb=0;38;2;152;195;121:*.cr=0;38;2;152;195;121:*.gz=4;38;2;86;182;194:*.sh=0;38;2;152;195;121:*.bz=4;38;2;86;182;194:*.nb=0;38;2;152;195;121:*.el=0;38;2;152;195;121:*.ml=0;38;2;152;195;121:*.cp=0;38;2;152;195;121:*.go=0;38;2;152;195;121:*.jl=0;38;2;152;195;121:*.xz=4;38;2;86;182;194:*.py=0;38;2;152;195;121:*.lo=0;38;2;102;102;102:*.bc=0;38;2;102;102;102:*.pl=0;38;2;152;195;121:*.ex=0;38;2;152;195;121:*.mn=0;38;2;152;195;121:*.pm=0;38;2;152;195;121:*.pp=0;38;2;152;195;121:*.hh=0;38;2;152;195;121:*.fs=0;38;2;152;195;121:*.hi=0;38;2;102;102;102:*.ps=0;38;2;224;108;117:*.vb=0;38;2;152;195;121:*.cs=0;38;2;152;195;121:*.md=0;38;2;229;192;123:*.kt=0;38;2;152;195;121:*.td=0;38;2;152;195;121:*.la=0;38;2;102;102;102:*.cc=0;38;2;152;195;121:*css=0;38;2;152;195;121:*.rs=0;38;2;152;195;121:*.di=0;38;2;152;195;121:*.ui=0;38;2;229;192;123:*.gv=0;38;2;152;195;121:*.rm=0;38;2;255;106;193:*.jpg=0;38;2;255;106;193:*.mp3=0;38;2;255;106;193:*.sxw=0;38;2;224;108;117:*.swf=0;38;2;255;106;193:*.csx=0;38;2;152;195;121:*.epp=0;38;2;152;195;121:*.aif=0;38;2;255;106;193:*.blg=0;38;2;102;102;102:*.eps=0;38;2;255;106;193:*.fon=0;38;2;255;106;193:*.txt=0;38;2;229;192;123:*.mid=0;38;2;255;106;193:*.bag=4;38;2;86;182;194:*.rtf=0;38;2;224;108;117:*.tbz=4;38;2;86;182;194:*.tsx=0;38;2;152;195;121:*.dmg=4;38;2;86;182;194:*.ogg=0;38;2;255;106;193:*.m4v=0;38;2;255;106;193:*.psd=0;38;2;255;106;193:*.def=0;38;2;152;195;121:*.gif=0;38;2;255;106;193:*.ics=0;38;2;224;108;117:*.tgz=4;38;2;86;182;194:*.mkv=0;38;2;255;106;193:*.log=0;38;2;102;102;102:*.com=1;38;2;224;108;117:*.erl=0;38;2;152;195;121:*.aux=0;38;2;102;102;102:*TODO=1:*.exe=1;38;2;224;108;117:*.tex=0;38;2;152;195;121:*.dpr=0;38;2;152;195;121:*.h++=0;38;2;152;195;121:*.ttf=0;38;2;255;106;193:*.c++=0;38;2;152;195;121:*.ico=0;38;2;255;106;193:*.bsh=0;38;2;152;195;121:*.xlr=0;38;2;224;108;117:*.avi=0;38;2;255;106;193:*.cxx=0;38;2;152;195;121:*.sql=0;38;2;152;195;121:*.tif=0;38;2;255;106;193:*.sty=0;38;2;102;102;102:*.kex=0;38;2;224;108;117:*.fnt=0;38;2;255;106;193:*.ppt=0;38;2;224;108;117:*.flv=0;38;2;255;106;193:*.hxx=0;38;2;152;195;121:*.apk=4;38;2;86;182;194:*.doc=0;38;2;224;108;117:*.rpm=4;38;2;86;182;194:*.inc=0;38;2;152;195;121:*.awk=0;38;2;152;195;121:*.vim=0;38;2;152;195;121:*.pid=0;38;2;102;102;102:*.asa=0;38;2;152;195;121:*.sxi=0;38;2;224;108;117:*.exs=0;38;2;152;195;121:*.nix=0;38;2;229;192;123:*.ipp=0;38;2;152;195;121:*.pyc=0;38;2;102;102;102:*.fsi=0;38;2;152;195;121:*.toc=0;38;2;102;102;102:*.wma=0;38;2;255;106;193:*.zip=4;38;2;86;182;194:*.htm=0;38;2;229;192;123:*.yml=0;38;2;229;192;123:*.pod=0;38;2;152;195;121:*.png=0;38;2;255;106;193:*.tmp=0;38;2;102;102;102:*.jar=4;38;2;86;182;194:*.tml=0;38;2;229;192;123:*.kts=0;38;2;152;195;121:*.elm=0;38;2;152;195;121:*hgrc=0;38;2;152;195;121:*.mir=0;38;2;152;195;121:*.vob=0;38;2;255;106;193:*.zst=4;38;2;86;182;194:*.bak=0;38;2;102;102;102:*.swp=0;38;2;102;102;102:*.php=0;38;2;152;195;121:*.tcl=0;38;2;152;195;121:*.vcd=4;38;2;86;182;194:*.bat=1;38;2;224;108;117:*.mov=0;38;2;255;106;193:*.wav=0;38;2;255;106;193:*.rar=4;38;2;86;182;194:*.clj=0;38;2;152;195;121:*.xmp=0;38;2;229;192;123:*.bmp=0;38;2;255;106;193:*.odp=0;38;2;224;108;117:*.sbt=0;38;2;152;195;121:*.bz2=4;38;2;86;182;194:*.git=0;38;2;102;102;102:*.ini=0;38;2;229;192;123:*.bbl=0;38;2;102;102;102:*.mli=0;38;2;152;195;121:*.pdf=0;38;2;224;108;117:*.wmv=0;38;2;255;106;193:*.ods=0;38;2;224;108;117:*.xls=0;38;2;224;108;117:*.out=0;38;2;102;102;102:*.pro=0;38;2;152;195;121:*.ilg=0;38;2;102;102;102:*.ltx=0;38;2;152;195;121:*.cgi=0;38;2;152;195;121:*.lua=0;38;2;152;195;121:*.img=4;38;2;86;182;194:*.htc=0;38;2;152;195;121:*.inl=0;38;2;152;195;121:*.xml=0;38;2;229;192;123:*.fls=0;38;2;102;102;102:*.otf=0;38;2;255;106;193:*.ppm=0;38;2;255;106;193:*.mpg=0;38;2;255;106;193:*.m4a=0;38;2;255;106;193:*.rst=0;38;2;229;192;123:*.hpp=0;38;2;152;195;121:*.mp4=0;38;2;255;106;193:*.dll=1;38;2;224;108;117:*.bin=4;38;2;86;182;194:*.bib=0;38;2;229;192;123:*.pbm=0;38;2;255;106;193:*.ind=0;38;2;102;102;102:*.dox=0;38;2;152;195;121:*.deb=4;38;2;86;182;194:*.idx=0;38;2;102;102;102:*.pps=0;38;2;224;108;117:*.bst=0;38;2;229;192;123:*.zsh=0;38;2;152;195;121:*.xcf=0;38;2;255;106;193:*.fsx=0;38;2;152;195;121:*.arj=4;38;2;86;182;194:*.tar=4;38;2;86;182;194:*.ps1=0;38;2;152;195;121:*.pgm=0;38;2;255;106;193:*.odt=0;38;2;224;108;117:*.pas=0;38;2;152;195;121:*.dot=0;38;2;152;195;121:*.bcf=0;38;2;102;102;102:*.iso=4;38;2;86;182;194:*.pkg=4;38;2;86;182;194:*.svg=0;38;2;255;106;193:*.csv=0;38;2;229;192;123:*.cpp=0;38;2;152;195;121:*.cfg=0;38;2;229;192;123:*.gvy=0;38;2;152;195;121:*.lock=0;38;2;102;102;102:*.h264=0;38;2;255;106;193:*.make=0;38;2;152;195;121:*.tiff=0;38;2;255;106;193:*.java=0;38;2;152;195;121:*.orig=0;38;2;102;102;102:*.html=0;38;2;229;192;123:*.flac=0;38;2;255;106;193:*.xlsx=0;38;2;224;108;117:*.jpeg=0;38;2;255;106;193:*.psd1=0;38;2;152;195;121:*.yaml=0;38;2;229;192;123:*.fish=0;38;2;152;195;121:*.pptx=0;38;2;224;108;117:*.epub=0;38;2;224;108;117:*.diff=0;38;2;152;195;121:*.rlib=0;38;2;102;102;102:*.docx=0;38;2;224;108;117:*.hgrc=0;38;2;152;195;121:*.less=0;38;2;152;195;121:*.mpeg=0;38;2;255;106;193:*.json=0;38;2;229;192;123:*.bash=0;38;2;152;195;121:*.dart=0;38;2;152;195;121:*.conf=0;38;2;229;192;123:*.psm1=0;38;2;152;195;121:*.tbz2=4;38;2;86;182;194:*.lisp=0;38;2;152;195;121:*.toml=0;38;2;229;192;123:*.purs=0;38;2;152;195;121:*passwd=0;38;2;229;192;123:*shadow=0;38;2;229;192;123:*.class=0;38;2;102;102;102:*.xhtml=0;38;2;229;192;123:*.swift=0;38;2;152;195;121:*.cache=0;38;2;102;102;102:*.scala=0;38;2;152;195;121:*.toast=4;38;2;86;182;194:*.patch=0;38;2;152;195;121:*.cabal=0;38;2;152;195;121:*.dyn_o=0;38;2;102;102;102:*.mdown=0;38;2;229;192;123:*.cmake=0;38;2;152;195;121:*.ipynb=0;38;2;152;195;121:*.shtml=0;38;2;229;192;123:*README=0;38;2;40;44;52;48;2;229;192;123:*.ignore=0;38;2;152;195;121:*.dyn_hi=0;38;2;102;102;102:*INSTALL=0;38;2;40;44;52;48;2;229;192;123:*TODO.md=1:*LICENSE=0;38;2;153;153;153:*.config=0;38;2;229;192;123:*COPYING=0;38;2;153;153;153:*.groovy=0;38;2;152;195;121:*.gradle=0;38;2;152;195;121:*.matlab=0;38;2;152;195;121:*.flake8=0;38;2;152;195;121:*setup.py=0;38;2;152;195;121:*.desktop=0;38;2;229;192;123:*.gemspec=0;38;2;152;195;121:*Doxyfile=0;38;2;152;195;121:*TODO.txt=1:*Makefile=0;38;2;152;195;121:*.fdignore=0;38;2;152;195;121:*.DS_Store=0;38;2;102;102;102:*configure=0;38;2;152;195;121:*.rgignore=0;38;2;152;195;121:*COPYRIGHT=0;38;2;153;153;153:*.markdown=0;38;2;229;192;123:*.cmake.in=0;38;2;152;195;121:*README.md=0;38;2;40;44;52;48;2;229;192;123:*.kdevelop=0;38;2;152;195;121:*SConscript=0;38;2;152;195;121:*SConstruct=0;38;2;152;195;121:*CODEOWNERS=0;38;2;152;195;121:*INSTALL.md=0;38;2;40;44;52;48;2;229;192;123:*.gitignore=0;38;2;152;195;121:*README.txt=0;38;2;40;44;52;48;2;229;192;123:*.localized=0;38;2;102;102;102:*.scons_opt=0;38;2;102;102;102:*Dockerfile=0;38;2;229;192;123:*.gitconfig=0;38;2;152;195;121:*MANIFEST.in=0;38;2;152;195;121:*.synctex.gz=0;38;2;102;102;102:*Makefile.in=0;38;2;102;102;102:*Makefile.am=0;38;2;152;195;121:*INSTALL.txt=0;38;2;40;44;52;48;2;229;192;123:*.gitmodules=0;38;2;152;195;121:*.travis.yml=0;38;2;152;195;121:*LICENSE-MIT=0;38;2;153;153;153:*configure.ac=0;38;2;152;195;121:*appveyor.yml=0;38;2;152;195;121:*.applescript=0;38;2;152;195;121:*.fdb_latexmk=0;38;2;102;102;102:*CONTRIBUTORS=0;38;2;40;44;52;48;2;229;192;123:*.clang-format=0;38;2;152;195;121:*.gitattributes=0;38;2;152;195;121:*CMakeCache.txt=0;38;2;102;102;102:*CMakeLists.txt=0;38;2;152;195;121:*LICENSE-APACHE=0;38;2;153;153;153:*CONTRIBUTORS.md=0;38;2;40;44;52;48;2;229;192;123:*requirements.txt=0;38;2;152;195;121:*.sconsign.dblite=0;38;2;102;102;102:*CONTRIBUTORS.txt=0;38;2;40;44;52;48;2;229;192;123:*package-lock.json=0;38;2;102;102;102:*.CFUserTextEncoding=0;38;2;102;102;102"
