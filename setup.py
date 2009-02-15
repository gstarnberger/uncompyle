#!/usr/bin/env python

"""Setup script for the 'unpyc' distribution."""

from distutils.core import setup, Extension

setup (name = "unpyc",
       version = "1.0",
       description = "Python byte-code to source-code converter",
       author = "Hartmut Goebel",
       author_email = "hartmut@oberon.noris.de",
       url = "http://code.google.com/p/unpyc/",
       packages=['unpyc'],
       scripts=['scripts/unpyc'],
       ext_modules = [Extension('unpyc/marshal_20',
                                ['unpyc/marshal_20.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_21',
                                ['unpyc/marshal_21.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_22',
                                ['unpyc/marshal_22.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_23',
                                ['unpyc/marshal_23.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_24',
                                ['unpyc/marshal_24.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_25',
                                ['unpyc/marshal_25.c'],
                                define_macros=[]),
                      Extension('unpyc/marshal_26',
                                ['unpyc/marshal_26.c'],
                                define_macros=[]),
                      ]
      )
