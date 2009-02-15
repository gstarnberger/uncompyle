#  Copyright (c) 1999 John Aycock
#  Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#  Copyright (c) 2005 by Dan Pascu <dan@windowmaker.org>
#
#  See main module for license.
#

__all__ = ['Token', 'Scanner', 'getscanner']

import types

class Token:
    """
    Class representing a byte-code token.
    
    A byte-code token is equivalent to the contents of one line
    as output by dis.dis().
    """
    def __init__(self, type, attr=None, pattr=None, offset=-1):
        self.type = intern(type)
        self.attr = attr
        self.pattr = pattr
        self.offset = offset
        
    def __cmp__(self, o):
        if isinstance(o, Token):
            # both are tokens: compare type and pattr 
            return cmp(self.type, o.type) or cmp(self.pattr, o.pattr)
        else:
            return cmp(self.type, o)

    def __repr__(self):		return str(self.type)
    def __str__(self):
        pattr = self.pattr or ''
        return '%s\t%-17s %r' % (self.offset, self.type, pattr)
    def __hash__(self):		return hash(self.type)
    def __getitem__(self, i):	raise IndexError


class Code:
    """
    Class for representing code-objects.

    This is similar to the original code object, but additionally
    the diassembled code is stored in the attribute '_tokens'.
    """
    def __init__(self, co, scanner):
        for i in dir(co):
            if i.startswith('co_'):
                setattr(self, i, getattr(co, i))
        self._tokens, self._customize = scanner.disassemble(co)

class Scanner:
    def __init__(self, version):
        self.__version = version

        from sys import version_info
        self.__pyversion = float('%d.%d' % version_info[0:2])

        # use module 'dis' for this version
        import dis_files
        self.dis = dis_files.by_version[version]
        self.resetTokenClass()

        dis = self.dis
        self.JUMP_OPs = map(lambda op: dis.opname[op],
                            dis.hasjrel + dis.hasjabs)

        # setup an opmap if we don't have one
        try:
            dis.opmap
        except:
            opmap = {}
            opname = dis.opname
            for i in range(len(opname)):
                opmap[opname[i]] = i
            dis.opmap = opmap
        copmap = {}
        for i in range(len(dis.cmp_op)):
            copmap[dis.cmp_op[i]] = i
        dis.copmap = copmap

    def setShowAsm(self, showasm, out=None):
        self.showasm = showasm
        self.out = out
            
    def setTokenClass(self, tokenClass):
        assert type(tokenClass) == types.ClassType
        self.Token = tokenClass
        
    def resetTokenClass(self):
        self.setTokenClass(Token)
        
    def disassemble(self, co):
        """
        Disassemble a code object, returning a list of 'Token'.

        The main part of this procedure is modelled after
        dis.disassemble().
        """
        rv = []
        customize = {}
        dis = self.dis # shortcut
        Token = self.Token # shortcut

        code = co.co_code
        cf = self.find_jump_targets(code)
        n = len(code)
        i = 0
        extended_arg = 0
        free = None
        while i < n:
            offset = i
            if cf.has_key(offset):
                for j in range(cf[offset]):
                    rv.append(Token('COME_FROM',
                                    offset="%s_%d" % (offset, j) ))

            c = code[i]
            op = ord(c)
            opname = dis.opname[op]
            i += 1
            oparg = None; pattr = None
            if op >= dis.HAVE_ARGUMENT:
                oparg = ord(code[i]) + ord(code[i+1]) * 256 + extended_arg
                extended_arg = 0
                i += 2
                if op == dis.EXTENDED_ARG:
                    extended_arg = oparg * 65536L
                if op in dis.hasconst:
                    const = co.co_consts[oparg]
                    if type(const) == types.CodeType:
                        oparg = const
                        if const.co_name == '<lambda>':
                            assert opname == 'LOAD_CONST'
                            opname = 'LOAD_LAMBDA'
                        # verify uses 'pattr' for comparism, since 'attr'
                        # now holds Code(const) and thus can not be used
                        # for comparism (todo: think about changing this)
                        #pattr = 'code_object @ 0x%x %s->%s' %\
                        #	(id(const), const.co_filename, const.co_name)
                        pattr = 'code_object ' + const.co_name
                    else:
                        pattr = const
                elif op in dis.hasname:
                    pattr = co.co_names[oparg]
                elif op in dis.hasjrel:
                    pattr = repr(i + oparg)
                elif op in dis.hasjabs:
                    pattr = repr(oparg)
                elif op in dis.haslocal:
                    pattr = co.co_varnames[oparg]
                elif op in dis.hascompare:
                    pattr = dis.cmp_op[oparg]
                elif op in dis.hasfree:
                    if free is None:
                        free = co.co_cellvars + co.co_freevars
                    pattr = free[oparg]

            if opname == 'SET_LINENO':
                continue
            elif opname in ('BUILD_LIST', 'BUILD_TUPLE', 'BUILD_SLICE',
                            'UNPACK_LIST', 'UNPACK_TUPLE', 'UNPACK_SEQUENCE',
                            'MAKE_FUNCTION', 'CALL_FUNCTION', 'MAKE_CLOSURE',
                            'CALL_FUNCTION_VAR', 'CALL_FUNCTION_KW',
                            'CALL_FUNCTION_VAR_KW', 'DUP_TOPX',
                            ):
                opname = '%s_%d' % (opname, oparg)
                customize[opname] = oparg

            rv.append(Token(opname, oparg, pattr, offset))

        if self.showasm:
            out = self.out # shortcut
            for t in rv:
                print >>out, t
            print >>out

        return rv, customize

    def __get_target(self, code, pos, op=None):
        if op is None:
            op = ord(code[pos])
        target = ord(code[pos+1]) + ord(code[pos+2]) * 256
        if op in self.dis.hasjrel:
            target += pos + 3
        return target

    def __first_instr(self, code, start, end, instr, target=None, exact=True):
        """
        Find the first <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely if exact
        is True, or if exact is False, the instruction which has a target
        closest to <target> will be returned.

        Return index to it or None if not found.
        """

        assert(start>=0 and end<len(code))

        HAVE_ARGUMENT = self.dis.HAVE_ARGUMENT

        try:    instr[0]
        except: instr = [instr]

        pos = None
        distance = len(code)
        i = start
        while i < end:
            op = ord(code[i])
            if op in instr:
                if target is None:
                    return i
                dest = self.__get_target(code, i, op)
                if dest == target:
                    return i
                elif not exact:
                    _distance = abs(target - dest)
                    if _distance < distance:
                        distance = _distance
                        pos = i
            if op < HAVE_ARGUMENT:
                i += 1
            else:
                i += 3
        return pos

    def __last_instr(self, code, start, end, instr, target=None, exact=True):
        """
        Find the last <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely if exact
        is True, or if exact is False, the instruction which has a target
        closest to <target> will be returned.

        Return index to it or None if not found.
        """

        assert(start>=0 and end<len(code))

        HAVE_ARGUMENT = self.dis.HAVE_ARGUMENT

        try:    instr[0]
        except: instr = [instr]

        pos = None
        distance = len(code)
        i = start
        while i < end:
            op = ord(code[i])
            if op in instr:
                if target is None:
                    pos = i
                else:
                    dest = self.__get_target(code, i, op)
                    if dest == target:
                        distance = 0
                        pos = i
                    elif not exact:
                        _distance = abs(target - dest)
                        if _distance <= distance:
                            distance = _distance
                            pos = i
            if op < HAVE_ARGUMENT:
                i += 1
            else:
                i += 3
        return pos

    def __all_instr(self, code, start, end, instr, target=None):
        """
        Find all <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely.

        Return a list with indexes to them or [] if none found.
        """

        assert(start>=0 and end<len(code))

        HAVE_ARGUMENT = self.dis.HAVE_ARGUMENT

        try:    instr[0]
        except: instr = [instr]

        result = []
        i = start
        while i < end:
            op = ord(code[i])
            if op in instr:
                if target is None:
                    result.append(i)
                elif target == self.__get_target(code, i, op):
                    result.append(i)
            if op < HAVE_ARGUMENT:
                i += 1
            else:
                i += 3
        return result

    def __next_except_jump(self, code, start, end, target):
        """
        Return the next jump that was generated by an except SomeException:
        construct in a try...except...else clause or None if not found.
        """
        HAVE_ARGUMENT = self.dis.HAVE_ARGUMENT
        JUMP_FORWARD  = self.dis.opmap['JUMP_FORWARD']
        JUMP_ABSOLUTE = self.dis.opmap['JUMP_ABSOLUTE']
        END_FINALLY   = self.dis.opmap['END_FINALLY']
        POP_TOP       = self.dis.opmap['POP_TOP']
        DUP_TOP       = self.dis.opmap['DUP_TOP']
        try:    SET_LINENO = self.dis.opmap['SET_LINENO']
        except: SET_LINENO = None

        lookup = [JUMP_ABSOLUTE, JUMP_FORWARD]
        while start < end:
            jmp = self.__first_instr(code, start, end, lookup, target)
            if jmp is None:
                return None
            if jmp == end-3:
                return jmp
            after = jmp + 3
            ops = [None, None, None, None]
            opp = [0, 0, 0, 0]
            pos = 0
            x = jmp+3
            while x <= end and pos < 4:
                op = ord(code[x])
                if op == SET_LINENO:
                    x += 3
                    continue
                elif op >= HAVE_ARGUMENT:
                    break
                ops[pos] = op
                opp[pos] = x
                pos += 1
                x += 1
            if ops[0] == POP_TOP and ops[1] == END_FINALLY and opp[1] == end:
                return jmp
            if ops[0] == POP_TOP and ops[1] == DUP_TOP:
                return jmp
            if ops[0] == ops[1] == ops[2] == ops[3] == POP_TOP:
                return jmp
            start = jmp + 3
        return None

    def __list_comprehension(self, code, pos, op=None):
        """
        Determine if there is a list comprehension structure starting at pos
        """
        BUILD_LIST = self.dis.opmap['BUILD_LIST']
        DUP_TOP    = self.dis.opmap['DUP_TOP']
        LOAD_ATTR  = self.dis.opmap['LOAD_ATTR']
        if op is None:
            op = ord(code[pos])
        if op != BUILD_LIST:
            return 0
        try:
            elems = ord(code[pos+1]) + ord(code[pos+2])*256
            codes = (op, elems, ord(code[pos+3]), ord(code[pos+4]))
        except IndexError:
            return 0
        return (codes==(BUILD_LIST, 0, DUP_TOP, LOAD_ATTR))

    def __ignore_if(self, code, pos):
        """
        Return true if this 'if' is to be ignored.
        """
        POP_TOP      = self.dis.opmap['POP_TOP']
        COMPARE_OP   = self.dis.opmap['COMPARE_OP']
        EXCEPT_MATCH = self.dis.copmap['exception match']

        ## If that was added by a while loop
        if pos in self.__ignored_ifs:
            return 1

        # Check if we can test only for POP_TOP for this -Dan
        # Maybe need to be done as above (skip SET_LINENO's)
        if (ord(code[pos-3])==COMPARE_OP and
            (ord(code[pos-2]) + ord(code[pos-1])*256)==EXCEPT_MATCH and
            ord(code[pos+3])==POP_TOP and
            ord(code[pos+4])==POP_TOP and
            ord(code[pos+5])==POP_TOP and
            ord(code[pos+6])==POP_TOP):
            return 1 ## Exception match
        return 0

    def __fix_parent(self, code, target, parent):
        """Fix parent boundaries if needed"""
        JUMP_ABSOLUTE = self.dis.opmap['JUMP_ABSOLUTE']
        start = parent['start']
        end = parent['end']
        ## Map the second start point for 'while 1:' in python 2.3+ to start
        try:    target = self.__while1[target]
        except: pass
        if target >= start or end-start < 3 or target not in self.__loops:
            return
        if ord(code[end-3])==JUMP_ABSOLUTE:
            cont_target = self.__get_target(code, end-3, JUMP_ABSOLUTE)
            if target == cont_target:
                parent['end'] = end-3

    def __restrict_to_parent(self, target, parent):
        """Restrict pos to parent boundaries."""
        if not (parent['start'] < target < parent['end']):
            target = parent['end']
        return target

    def __detect_structure(self, code, pos, op=None):
        """
        Detect structures and their boundaries to fix optimizied jumps
        in python2.3+
        """

        # TODO: check the struct boundaries more precisely -Dan

        SETUP_LOOP    = self.dis.opmap['SETUP_LOOP']
        BUILD_LIST    = self.dis.opmap['BUILD_LIST']
        FOR_ITER      = self.dis.opmap['FOR_ITER']
        GET_ITER      = self.dis.opmap['GET_ITER']
        SETUP_EXCEPT  = self.dis.opmap['SETUP_EXCEPT']
        JUMP_FORWARD  = self.dis.opmap['JUMP_FORWARD']
        JUMP_ABSOLUTE = self.dis.opmap['JUMP_ABSOLUTE']
        JUMP_IF_FALSE = self.dis.opmap['JUMP_IF_FALSE']
        JUMP_IF_TRUE  = self.dis.opmap['JUMP_IF_TRUE']
        END_FINALLY   = self.dis.opmap['END_FINALLY']
        POP_TOP       = self.dis.opmap['POP_TOP']
        try:    SET_LINENO = self.dis.opmap['SET_LINENO']
        except: SET_LINENO = None

        # Ev remove this test and make op a mandatory argument -Dan
        if op is None:
            op = ord(code[pos])

        ## Detect parent structure
        parent = self.__structs[0]
        start  = parent['start']
        end    = parent['end']
        for s in self.__structs:
            _start = s['start']
            _end   = s['end']
            if (_start <= pos < _end) and (_start >= start and _end < end):
                start  = _start
                end    = _end
                parent = s

        ## We need to know how many new structures were added in this run
        origStructCount = len(self.__structs)

        if op == SETUP_LOOP:
            start = pos+3
            if ord(code[start])==JUMP_FORWARD:
                ## This is a while 1 (has a particular structure)
                start = self.__get_target(code, start, JUMP_FORWARD)
                start = self.__restrict_to_parent(start, parent)
                self.__while1[pos+3] = start ## map between the 2 start points
            target = self.__get_target(code, pos, op)
            end    = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[pos] = end
            jump_back = self.__last_instr(code, start, end, JUMP_ABSOLUTE,
                                          start, False)
            assert(jump_back is not None)
            target = self.__get_target(code, jump_back, JUMP_ABSOLUTE)
            i = target
            while i < jump_back and ord(code[i])==SET_LINENO:
                i += 3
            if ord(code[i]) in (FOR_ITER, GET_ITER):
                loop_type = 'for'
            else:
                loop_type = 'while'
                lookup = [JUMP_IF_FALSE, JUMP_IF_TRUE]
                test = self.__first_instr(code, pos+3, jump_back, lookup)
                assert(test is not None)
                self.__ignored_ifs.append(test)
            self.__loops.append(target)
            self.__structs.append({'type': loop_type + '-loop',
                                   'start': target,
                                   'end':   jump_back})
            self.__structs.append({'type': loop_type + '-else',
                                   'start': jump_back+3,
                                   'end':   end})
        elif self.__list_comprehension(code, pos, op):
            get_iter = self.__first_instr(code, pos+7, end, GET_ITER)
            for_iter = self.__first_instr(code, get_iter, end, FOR_ITER)
            assert(get_iter is not None and for_iter is not None)
            start  = get_iter+1
            target = self.__get_target(code, for_iter, FOR_ITER)
            end    = self.__restrict_to_parent(target, parent)
            jump_back = self.__last_instr(code, start, end, JUMP_ABSOLUTE,
                                          start, False)
            assert(jump_back is not None)
            target = self.__get_target(code, jump_back, JUMP_ABSOLUTE)
            start = self.__restrict_to_parent(target, parent)
            self.__structs.append({'type': 'list-comprehension',
                                   'start': start,
                                   'end':   jump_back})
        elif op == SETUP_EXCEPT:
            start  = pos+3
            target = self.__get_target(code, pos, op)
            end    = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[pos] = end
            ## Add the try block
            self.__structs.append({'type':  'try',
                                   'start': start,
                                   'end':   end-4})
            ## Now isolate the except and else blocks
            start  = end
            target = self.__get_target(code, start-3)
            self.__fix_parent(code, target, parent)
            end    = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[start-3] = end

            end_finally = self.__last_instr(code, start, end, END_FINALLY)
            assert(end_finally is not None)
            lookup = [JUMP_ABSOLUTE, JUMP_FORWARD]
            jump_end = self.__last_instr(code, start, end, lookup)
            assert(jump_end is not None)

            target = self.__get_target(code, jump_end)
            end = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[jump_end] = end

            ## Add the try-else block
            self.__structs.append({'type':  'try-else',
                                   'start': end_finally+1,
                                   'end':   end})
            ## Add the except blocks
            i = start
            while i < end_finally:
                jmp = self.__next_except_jump(code, i, end_finally, target)
                if jmp is None:
                    break
                self.__structs.append({'type':  'except',
                                       'start': i,
                                       'end':   jmp})
                if target != end:
                    self.__fixed_jumps[jmp] = end
                i = jmp+3
        elif op == JUMP_ABSOLUTE:
            ## detect if we have a 'foo and bar and baz...' structure
            ## that was optimized (thus the presence of JUMP_ABSOLUTE)
            if pos in self.__fixed_jumps:
                return ## Already marked
            if parent['end'] - pos < 7:
                return
            next = (ord(code[pos+3]), ord(code[pos+6]))
            if next != (JUMP_IF_FALSE, POP_TOP):
                return

            end = self.__get_target(code, pos+3)
            ifs = self.__all_instr(code, pos, end, JUMP_IF_FALSE, end)

            ## Test if all JUMP_IF_FALSE we have found belong to the
            ## structure (may not be needed but it doesn't hurt)
            count = len(ifs)
            if count < 2:
                return
            for jif in ifs[1:]:
                before = ord(code[jif-3])
                after  = ord(code[jif+3])
                if (before not in (JUMP_FORWARD, JUMP_ABSOLUTE) or
                    after != POP_TOP):
                    return

            ## All tests passed. Perform fixes
            self.__ignored_ifs.extend(ifs)
            for i in range(count-1):
                self.__fixed_jumps[ifs[i]-3] = ifs[i+1]-3
        elif op in (JUMP_IF_FALSE, JUMP_IF_TRUE):
            if self.__ignore_if(code, pos):
                return
            start  = pos+4 ## JUMP_IF_FALSE/TRUE + POP_TOP
            target = self.__get_target(code, pos, op)
            if parent['start'] <= target < parent['end']:
                if ord(code[target-3]) in (JUMP_ABSOLUTE, JUMP_FORWARD):
                    if_end = self.__get_target(code, target-3)
                    self.__fix_parent(code, if_end, parent)
                    end = self.__restrict_to_parent(if_end, parent)
                    if if_end != end:
                        self.__fixed_jumps[target-3] = end
                    self.__structs.append({'type':  'if-then',
                                           'start': start,
                                           'end':   target-3})
                    self.__structs.append({'type':  'if-else',
                                           'start': target+1,
                                           'end':   end})

    def find_jump_targets(self, code):
        """
        Detect all offsets in a byte code which are jump targets.

        Return the list of offsets.

        This procedure is modelled after dis.findlables(), but here
        for each target the number of jumps are counted.
        """
        HAVE_ARGUMENT = self.dis.HAVE_ARGUMENT

        hasjrel = self.dis.hasjrel
        hasjabs = self.dis.hasjabs

        needFixing = (self.__pyversion >= 2.3)

        n = len(code)
        self.__structs = [{'type':  'root',
                           'start': 0,
                           'end':   n-1}]
        self.__loops = []  ## All loop entry points
        self.__while1 = {} ## 'while 1:' in python 2.3+ has another start point
        self.__fixed_jumps = {} ## Map fixed jumps to their real destination
        self.__ignored_ifs = [] ## JUMP_IF_XXXX's we should ignore

        targets = {}
        i = 0
        while i < n:
            op = ord(code[i])

            if needFixing:
                ## Determine structures and fix jumps for 2.3+
                self.__detect_structure(code, i, op)

            if op >= HAVE_ARGUMENT:
                label = self.__fixed_jumps.get(i)
                if label is None:
                    oparg = ord(code[i+1]) + ord(code[i+2]) * 256
                    if op in hasjrel:
                        label = i + 3 + oparg
                    elif op in hasjabs:
                        # todo: absolute jumps
                        pass
                if label is not None:
                    targets[label] = targets.get(label, 0) + 1
                i += 3
            else:
                i += 1
        return targets


__scanners = {}

def getscanner(version):
    if not __scanners.has_key(version):
        __scanners[version] = Scanner(version)
    return __scanners[version]

# local variables:
# tab-width: 4
