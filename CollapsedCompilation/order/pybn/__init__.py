# __init__.py - Package initialization for the bn package

#__version__ = "0.0.0"

import os, sys

# Add java stuff to path: is there a better/standard way to do this?
path = os.path.realpath(__file__)
path = os.path.dirname(path)
libpath = os.path.join(path,'java')
libpath = os.path.join(libpath,'lib')
if libpath not in sys.path: sys.path.append(libpath)
libpath = os.path.join(libpath,'inflib.jar')
if libpath not in sys.path: sys.path.append(libpath)

__all__ = ['core','engines','learn','net_io','network','util']
