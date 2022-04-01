import readline
import rlcompleter
import atexit
import os


histfile = os.path.join(os.environ['HOME'], '.cache/python/history')

try:
    readline.read_history_file(histfile)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)

try:
    from jedi.utils import setup_readline
except ImportError:
    pass
else:
    setup_readline()

try:
    from rich import pretty
    pretty.install()
except ImportError:
    pass
del os, histfile, readline, rlcompleter
