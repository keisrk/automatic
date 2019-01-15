from distutils.spawn import find_executable
import logging
import sys

def check_executable(bin):
    return find_executable(bin) is not None

def check_module(mod):
    return mod in sys.modules

class ToolChain(object):
    """Inspect whether the proper toolchain exists required in the
    build."""

    def __init__(self, tools,
                 label='executable',
                 check=check_executable):
        """Set toolchains."""
        self.tools = tools
        self.label = label
        self.check = check
        self.missing = None

    def sanitize(self):
        """Check if the environment has the toolchain"""
        self.missing = []

        logging.info('Checking {}'.format(self.label))

        for tool in self.tools:
            if not self.check(tool):
                logging.error(
                    'Missing {} {}'.format(self.label, tool)
                )
                self.missing.append(tool)

    def is_complete(self):
        """Return True if self.sanitize() was successful."""
        return self.missing == []

def check_toolchain(*tcs):
    """Start sanitizing the environment."""
    for tc in tcs:
        tc.sanitize()

    if not all([tc.is_complete() for tc in tcs]):
        logging.error('Sanitizing toolchain failed.')
