#!/usr/bin/env python

"""Setup script for the 'uncompyle' distribution."""

from distutils.core import setup, Extension

setup (name = "uncompyle",
       version = "1.1",
       description = "Python byte-code to source-code converter",
       author = "Hartmut Goebel",
       author_email = "hartmut@oberon.noris.de",
       url = "http://github.com/sysfrog/uncompyle",
       packages=['uncompyle'],
       scripts=['scripts/uncompyle'],
       ext_modules = [Extension('uncompyle/marshal_20',
                                ['uncompyle/marshal_20.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_21',
                                ['uncompyle/marshal_21.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_22',
                                ['uncompyle/marshal_22.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_23',
                                ['uncompyle/marshal_23.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_24',
                                ['uncompyle/marshal_24.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_25',
                                ['uncompyle/marshal_25.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_26',
                                ['uncompyle/marshal_26.c'],
                                define_macros=[]),
                      Extension('uncompyle/marshal_27',
                                ['uncompyle/marshal_27.c'],
                                define_macros=[])
                      ]
      )
