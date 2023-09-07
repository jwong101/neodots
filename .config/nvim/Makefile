fnl_files = fnl/autocmd.fnl
lua_files = lua/autocmd.lua

all: $(lua_files)

$(lua_files): %.lua: %.fnl
	fennel --compile
