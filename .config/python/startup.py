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
del os, histfile, readline, rlcompleter
