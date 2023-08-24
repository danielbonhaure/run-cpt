
class Error(Exception):
    """Base class for exceptions in this module."""


class ConfigError(Error):
    """Raised when a configuration value is wrong"""
    pass


class CPTRuntimeError(Error):
    """Raised when CPT throws errors at runtime"""
    pass
