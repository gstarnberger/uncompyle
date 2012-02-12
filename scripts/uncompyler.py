#!/usr/bin/env python2.7
# Mode: -*- python -*-
#
# Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#
"""
Usage: uncompyler [OPTIONS]... [ FILE | DIR]...

Examples:
  uncompyler      foo.pyc bar.pyc       # uncompyle foo.pyc, bar.pyc to stdout
  uncompyler -o . foo.pyc bar.pyc       # uncompyle to ./foo.dis and ./bar.dis
  uncompyler -o /tmp /usr/lib/python1.5 # uncompyle whole library

Options:
  -o <path>     output decompiled files to this path:
                if multiple input files are decompiled, the common prefix
                is stripped from these names and the remainder appended to
                <path>
                  uncompyler -o /tmp bla/fasel.pyc bla/foo.pyc
                    -> /tmp/fasel.dis, /tmp/foo.dis
                  uncompyler -o /tmp bla/fasel.pyc bar/foo.pyc
                    -> /tmp/bla/fasel.dis, /tmp/bar/foo.dis
                  uncompyler -o /tmp /usr/lib/python1.5
                    -> /tmp/smtplib.dis ... /tmp/lib-tk/FixTk.dis
  -c <file>     attempts a disassembly after compiling <file>
  -d            do not print timestamps
  -p <integer>  use <integer> number of processes
  -r            recurse directories looking for .pyc and .pyo files
  --verify      compare generated source with input byte-code
                (requires -o)
  --help        show this message

Debugging Options:
  --showasm   -a  include byte-code                  (disables --verify)
  --showast   -t  include AST (abstract syntax tree) (disables --verify)

Extensions of generated files:
  '.dis'             successfully decompiled (and verified if --verify)
  '.dis_unverified'  successfully decompile but --verify failed
  '.nodis'           uncompyle failed (contact author for enhancement)
"""
from threading import Thread
from multiprocessing import Process, Queue
from Queue import Empty
from uncompyle import main, verify

def process_func(src_base, out_base, codes, outfile, showasm, showast, do_verify, fqueue, rqueue):
    try:
      (tot_files, okay_files, failed_files, verify_failed_files) = (0,0,0,0)
      while 1:
          f = fqueue.get()
          if f == None:
              break
          (t, o, f, v) = \
              main(src_base, out_base, [f], codes, outfile, showasm, showast, do_verify)
          tot_files += t
          okay_files += o
          failed_files += f
          verify_failed_files += v
    except (Empty, KeyboardInterrupt, OSError):
      pass
    rqueue.put((tot_files, okay_files, failed_files, verify_failed_files))
    rqueue.close()

if __name__ == '__main__':
    Usage_short = \
    "decomyple [--help] [--verify] [--showasm] [--showast] [-o <path>] FILE|DIR..."

    import sys, os, getopt
    import os.path
    import time

    showasm = showast = do_verify = numproc = recurse_dirs = 0
    outfile = '-'
    out_base = None
    codes = []
    timestamp = True
    timestampfmt = "# %Y.%m.%d %H:%M:%S %Z"

    try:
        opts, files = getopt.getopt(sys.argv[1:], 'hatdro:c:p:',
                               ['help', 'verify', 'showast', 'showasm'])
    except getopt.GetoptError, e:
        print >>sys.stderr, '%s: %s' % (os.path.basename(sys.argv[0]), e)
        sys.exit(-1)    

    for opt, val in opts:
        if opt in ('-h', '--help'):
            print __doc__
            sys.exit(0)
        elif opt == '--verify':
            do_verify = 1
        elif opt in ('--showasm', '-a'):
            showasm = 1
            do_verify = 0
        elif opt in ('--showast', '-t'):
            showast = 1
            do_verify = 0
        elif opt == '-o':
            outfile = val
        elif opt == '-d':
            timestamp = False
        elif opt == '-c':
            codes.append(val)
        elif opt == '-p':
            numproc = int(val)
        elif opt == '-r':
            recurse_dirs = 1
        else:
            print opt
            print Usage_short
            sys.exit(1)

    # expand directory if specified
    if recurse_dirs:
        expanded_files = []
        for f in files:
            if os.path.isdir(f):
                for root, _, dir_files in os.walk(f):
                    for df in dir_files:
                        if df.endswith('.pyc') or df.endswith('.pyo'):
                            expanded_files.append(os.path.join(root, df))
        files = expanded_files

    # argl, commonprefix works on strings, not on path parts,
    # thus we must handle the case with files in 'some/classes'
    # and 'some/cmds'
    src_base = os.path.commonprefix(files)
    if src_base[-1:] != os.sep:
        src_base = os.path.dirname(src_base)
    if src_base:
        sb_len = len( os.path.join(src_base, '') )
        files = map(lambda f: f[sb_len:], files)
        del sb_len
        
    if outfile == '-':
        outfile = None # use stdout
    elif outfile and os.path.isdir(outfile):
        out_base = outfile; outfile = None
    elif outfile and len(files) > 1:
        out_base = outfile; outfile = None

    if timestamp:
        print time.strftime(timestampfmt)
    if numproc <= 1:
        try:
            result = main(src_base, out_base, files, codes, outfile, showasm, showast, do_verify)
            print '# decompiled %i files: %i okay, %i failed, %i verify failed' % result
        except (KeyboardInterrupt, OSError):
            pass
        except verify.VerifyCmpError:
            raise
    else:
        # create directories beforehand
        for f in files:
          try:
            os.makedirs(os.path.join(out_base, os.path.dirname(f)))
          except OSError:
            pass
        fqueue = Queue(len(files)+numproc)
        for f in files:
            fqueue.put(f)
        for i in range(numproc):
            fqueue.put(None)
            
        rqueue = Queue(numproc)
        
        try:
            procs = [Process(target=process_func, args=(src_base, out_base, codes, outfile, showasm, showast, do_verify, fqueue, rqueue)) for i in range(numproc)]
            for p in procs:
                p.start()
            for p in procs:
                p.join()
            try:
                (tot_files, okay_files, failed_files, verify_failed_files) = (0,0,0,0)
                while 1:
                    (t, o, f, v) = rqueue.get(False)
                    tot_files += t
                    okay_files += o
                    failed_files += f
                    verify_failed_files += v
            except Empty:
                pass
            print '# decompiled %i files: %i okay, %i failed, %i verify failed' % \
                  (tot_files, okay_files, failed_files, verify_failed_files)
        except (KeyboardInterrupt, OSError):
            pass
            

    if timestamp:
        print time.strftime(timestampfmt)
