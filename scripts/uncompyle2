#!/usr/bin/env python2.7
# Mode: -*- python -*-
#
# Copyright (c) 2000-2002 by hartmut Goebel <h.goebel@crazy-compilers.com>
#
"""
Usage: uncompyle2 [OPTIONS]... [ FILE | DIR]...

Examples:
  uncompyle2      foo.pyc bar.pyc       # decompile foo.pyc, bar.pyc to stdout
  uncompyle2 -o . foo.pyc bar.pyc       # decompile to ./foo.dis and ./bar.dis
  uncompyle2 -o /tmp /usr/lib/python1.5 # decompile whole library

Options:
  -o <path>     output decompiled files to this path:
                if multiple input files are decompiled, the common prefix
                is stripped from these names and the remainder appended to
                <path>
                  uncompyle -o /tmp bla/fasel.pyc bla/foo.pyc
                    -> /tmp/fasel.dis, /tmp/foo.dis
                  uncompyle -o /tmp bla/fasel.pyc bar/foo.pyc
                    -> /tmp/bla/fasel.dis, /tmp/bar/foo.dis
  -s            if multiple input files are decompiled, the common prefix
                is stripped from these names and the remainder appended to
                <path>
                  uncompyle -o /tmp /usr/lib/python1.5
                    -> /tmp/smtplib.dis ... /tmp/lib-tk/FixTk.dis
  -c <file>     attempts a disassembly after compiling <file>
  -d            do not print timestamps
  -m            use multiprocessing
  --py          use '.py' extension for generated files
  --norecur     don't recurse directories looking for .pyc and .pyo files
  --verify      compare generated source with input byte-code
                (requires -o)
  --help        show this message

Debugging Options:
  --showasm   -a  include byte-code                  (disables --verify)
  --showast   -t  include AST (abstract syntax tree) (disables --verify)

Extensions of generated files:
  '.pyc_dis' '.pyo_dis'   successfully decompiled (and verified if --verify)
  '.py'                   with --py option
    + '_unverified'       successfully decompile but --verify failed
    + '_failed'           uncompyle failed (contact author for enhancement)
"""

Usage_short = \
"uncompyle2 [--help] [--verify] [--showasm] [--showast] [-o <path>] FILE|DIR..."

import sys, os, getopt
if sys.version[:3] != '2.7':
	print >>sys.stderr, 'Error:  uncompyle2 requires Python 2.7.'
	sys.exit(-1)
from uncompyle2 import main, verify
import time
from multiprocessing import Process, Queue, cpu_count
from Queue import Empty

def process_func(fq, rq, src_base, out_base, codes, outfile, showasm, showast, do_verify, py, deob):
    try:
        (tot_files, okay_files, failed_files, verify_failed_files) = (0,0,0,0)
        while 1:
            f = fq.get()
            if f == None:
                break
            (t, o, f, v) = \
                main(src_base, out_base, [f], codes, outfile, showasm, showast, do_verify, py, deob)
            tot_files += t
            okay_files += o
            failed_files += f
            verify_failed_files += v
    except (Empty, KeyboardInterrupt):
        pass
    rq.put((tot_files, okay_files, failed_files, verify_failed_files))
    rq.close()

if __name__ == '__main__': ## for Windows multiprocessing 
        
    showasm = showast = do_verify = multi = norecur = strip_common_path = py = deob = 0
    outfile = '-'
    out_base = None
    codes = []
    timestamp = True
    timestampfmt = "# %Y.%m.%d %H:%M:%S %Z"

    try:
        opts, files = getopt.getopt(sys.argv[1:], 'hatdrmso:c:',
                               ['help', 'verify', 'showast', 'showasm', 'norecur', 'py', 'deob'])
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
        elif opt == '-m':
            multi = 1
        elif opt == '--norecur':
            norecur = 1
        elif opt == '-s':
            strip_common_path = 1
        elif opt == '--py':
            py = 1
        elif opt == '--deob':
            deob = 1
        else:
            print opt
            print Usage_short
            sys.exit(1)

    # expand directory if specified
    if not norecur:
        expanded_files = []
        for f in files:
            if os.path.isdir(f):
                for root, _, dir_files in os.walk(f):
                    for df in dir_files:
                        if df.endswith('.pyc') or df.endswith('.pyo'):
                            expanded_files.append(os.path.join(root, df))
            else:
                expanded_files.append(f)
        files = expanded_files

    # argl, commonprefix works on strings, not on path parts,
    # thus we must handle the case with files in 'some/classes'
    # and 'some/cmds'
    if strip_common_path:
        src_base = os.path.commonprefix(files)
        if src_base[-1:] != os.sep:
            src_base = os.path.dirname(src_base)
        if src_base:
            sb_len = len( os.path.join(src_base, '') )
            files = map(lambda f: f[sb_len:], files)
            del sb_len
    else:
        src_base = ''
        
    if outfile == '-':
        outfile = None # use stdout
    elif outfile and os.path.isdir(outfile):
        out_base = outfile; outfile = None
    elif outfile and len(files) > 1:
        out_base = outfile; outfile = None

    if timestamp:
        print time.strftime(timestampfmt)
    if not multi:
        try:
            result = main(src_base, out_base, files, codes, outfile,
                          showasm, showast, do_verify, py, deob)
            print '# decompiled %i files: %i okay, %i failed, %i verify failed' % result
        except (KeyboardInterrupt):
            pass
        except verify.VerifyCmpError:
            raise
    else:
        numproc = cpu_count()
        fqueue = Queue(len(files)+numproc)
        for f in files:
            fqueue.put(f)
        for i in range(numproc):
            fqueue.put(None)
            
        rqueue = Queue(numproc)
        
        try:
            procs = [Process(target=process_func,
                             args=(fqueue, rqueue, src_base, out_base, codes, outfile,
                                   showasm, showast, do_verify, py, deob))
                     for i in range(numproc)]
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
