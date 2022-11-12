"""Fix python's history file mess."""

import sys


def register_readline():
    """Readline configuration function.

    Overriding https://github.com/python/cpython/blob/v3.7.0b5/Lib/site.py#L405
    """
    import atexit
    import os

    try:
        import readline
        from pathlib import Path
    except ImportError:
        return

    cache_home = (
        Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")) / "python"
    ).expanduser()
    cache_home.mkdir(parents=True, exist_ok=True)
    history = str(cache_home / "history")

    try:
        readline.read_history_file(history)
    except OSError:
        pass

    atexit.register(readline.write_history_file, history)


sys.__interactivehook__ = register_readline
