#!/usr/bin/env python

"""Setup script for the 'uncompyle' distribution."""

from distutils.core import setup, Extension

setup (name = "uncompyle",
       version = "1.1",
       description = "Python byte-code to source-code converter",
       author = "Hartmut Goebel",
       author_email = "hartmut@oberon.noris.de",
       url = "http://github.com/gstarnberger/uncompyle",
       packages=['uncompyle'],
       scripts=['scripts/uncompyler.py'],
      )
