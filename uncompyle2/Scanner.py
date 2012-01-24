#  Copyright (c) 1999 John Aycock
#  Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#  Copyright (c) 2005 by Dan Pascu <dan@windowmaker.org>
#
#  See main module for license.
#

__all__ = ['Token', 'Scanner', 'getscanner']

import types
import dis

globals().update(dis.opmap)


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
    def __init__(self, co, scanner, classname=None):
        for i in dir(co):
            if i.startswith('co_'):
                setattr(self, i, getattr(co, i))
        self._tokens, self._customize = scanner.disassemble(co, classname)

class Scanner:
    def __init__(self, version):
        self.__version = version

        from sys import version_info
        self.__pyversion = float('%d.%d' % version_info[0:2])

        self.resetTokenClass()

        self.JUMP_OPs = map(lambda op: dis.opname[op],
                            dis.hasjrel + dis.hasjabs)

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
        
    def disassemble(self, co, classname=None):
        """
        Disassemble a code object, returning a list of 'Token'.

        The main part of this procedure is modelled after
        dis.disassemble().
        """
        rv = []
        customize = {}
        Token = self.Token # shortcut
        code = co.co_code
        n = len(code)
        self.prev = [0]
        i=0
        while i < n:
            c = code[i]
            op = ord(code[i])
            if op >= dis.HAVE_ARGUMENT:
                self.prev.append(i)
                self.prev.append(i)
                self.prev.append(i)
                i = i + 3
            else:
                self.prev.append(i)
                i = i + 1
                
        self.lines = []
        self.if_lines = {}
        j = 0
        linestarts = list(dis.findlinestarts(co))
        (prev_start_byte, prev_line_no) = linestarts[0]
        for (start_byte, line_no) in linestarts[1:]:
            while j < start_byte:
                self.lines.append((prev_line_no, start_byte))
                j += 1
            last_op = ord(code[self.prev[start_byte]])
            if last_op in (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE):
                self.if_lines[prev_line_no] = True
            else:
                self.if_lines[prev_line_no] = False
            (prev_start_byte, prev_line_no) = (start_byte, line_no)
        while j < n:
            self.lines.append((prev_line_no, n))
            j+=1
        self.if_lines[prev_line_no] = False
        
        cf = self.find_jump_targets(code)

        if classname:
            classname = '_' + classname.lstrip('_') + '__'
            def unmangle(name):
                if name.startswith(classname) and name[-2:] != '__':
                    return name[len(classname) - 2:]
                return name
                
            free = [ unmangle(name) for name in (co.co_cellvars + co.co_freevars) ]
            names = [ unmangle(name) for name in co.co_names ]
            varnames = [ unmangle(name) for name in co.co_varnames ]
        else:
            free = co.co_cellvars + co.co_freevars
            names = co.co_names
            varnames = co.co_varnames

        i = 0
        extended_arg = 0
        while i < n:
            offset = i
            k = 0
            if cf.has_key(offset):
                for j in cf[offset]:
                    rv.append(Token('COME_FROM', None, repr(j),
                                    offset="%s_%d" % (offset, k) ))
                    k += 1
                    
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
                        elif const.co_name == '<genexpr>':
                            opname = 'LOAD_GENEXPR'
                        elif const.co_name == '<dictcomp>':
                            opname = 'LOAD_DICTCOMP'
                        elif const.co_name == '<setcomp>':
                            opname = 'LOAD_SETCOMP'
                        # verify uses 'pattr' for comparism, since 'attr'
                        # now holds Code(const) and thus can not be used
                        # for comparism (todo: think about changing this)
                        #pattr = 'code_object @ 0x%x %s->%s' %\
                        #	(id(const), const.co_filename, const.co_name)
                        pattr = '<code_object ' + const.co_name + '>'
                    else:
                        pattr = const
                elif op in dis.hasname:
                    pattr = names[oparg]
                elif op in dis.hasjrel:
                    pattr = repr(i + oparg)
                elif op in dis.hasjabs:
                    pattr = repr(oparg)
                elif op in dis.haslocal:
                    pattr = varnames[oparg]
                elif op in dis.hascompare:
                    pattr = dis.cmp_op[oparg]
                elif op in dis.hasfree:
                    pattr = free[oparg]

            if opname in ('BUILD_LIST', 'BUILD_TUPLE', 'BUILD_SET', 'BUILD_SLICE',
                            'UNPACK_LIST', 'UNPACK_TUPLE', 'UNPACK_SEQUENCE',
                            'MAKE_FUNCTION', 'CALL_FUNCTION', 'MAKE_CLOSURE',
                            'CALL_FUNCTION_VAR', 'CALL_FUNCTION_KW',
                            'CALL_FUNCTION_VAR_KW', 'DUP_TOPX',
                            ):
                # CE - Hack for >= 2.5
                #      Now all values loaded via LOAD_CLOSURE are packed into
                #      a tuple before calling MAKE_CLOSURE.
                if opname == 'BUILD_TUPLE' and \
                   dis.opname[ord(code[offset-3])] == 'LOAD_CLOSURE':
                    continue
                else:
                    opname = '%s_%d' % (opname, oparg)
                    if opname not in ('BUILD_SLICE_2', 'BUILD_SLICE_3'):
                        customize[opname] = oparg
            elif opname == 'JUMP_ABSOLUTE':
                target = self.__get_target(code, offset)
                if target < offset:
                    opname = 'JUMP_BACK'

            elif opname == 'LOAD_GLOBAL':
                try:
                    if pattr == 'AssertionError' and rv and rv[-1] == 'POP_JUMP_IF_TRUE':
                        opname = 'LOAD_ASSERT'
                except AttributeError:
                    pass

            elif opname == 'IMPORT_NAME':
                if pattr == '':
                    pattr = '.'

            rv.append(Token(opname, oparg, pattr, offset))

            if self.__jump_back_else.get(offset, False):
                rv.append(Token('JUMP_BACK_ELSE', None, None, 
                    offset="%s_" % offset ))
            
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
        if op in dis.hasjrel:
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

        assert(start>=0 and end<=len(code))

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

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

        if not (start>=0 and end<=len(code)):
            return None

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

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

        assert(start>=0 and end<=len(code))

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

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
        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

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
                if op >= HAVE_ARGUMENT:
                    break
                ops[pos] = op
                opp[pos] = x
                pos += 1
                x += 1
            if ops[0] == END_FINALLY and opp[0] == end:
                return jmp
            if ops[0] == DUP_TOP:
                return jmp
            if ops[0] == ops[1] == ops[2] == POP_TOP:
                return jmp
            start = jmp + 3
        return None

    def __fix_parent(self, code, target, parent):
        """Fix parent boundaries if needed"""
        start = parent['start']
        end = parent['end']

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
            if (_start <= pos < _end) and (_start >= start and _end <= end):
                start  = _start
                end    = _end
                parent = s

        ## We need to know how many new structures were added in this run
        origStructCount = len(self.__structs)

        if op == SETUP_LOOP:
            start = pos+3
            target = self.__get_target(code, pos, op)
            end    = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[pos] = end
            
            (line_no, next_line_byte) = self.lines[pos]
            jump_back = self.__last_instr(code, start, end, JUMP_ABSOLUTE,
                                          next_line_byte, False)
            if not jump_back:
                return

            if self.__get_target(code, jump_back) >= next_line_byte:
                jump_back = self.__last_instr(code, start, end, JUMP_ABSOLUTE,
                                          start, False)

                
            if end > jump_back+4 and ord(code[end]) in (JUMP_FORWARD, JUMP_ABSOLUTE):
                if ord(code[jump_back+4]) in (JUMP_ABSOLUTE,):
                    if self.__get_target(code, jump_back+4) == self.__get_target(code, end):
                        self.__fixed_jumps[pos] = jump_back+4
                        end = jump_back+4
            elif target < pos:
                self.__fixed_jumps[pos] = jump_back+4
                end = jump_back+4
             
            target = self.__get_target(code, jump_back, JUMP_ABSOLUTE)

            if ord(code[target]) in (FOR_ITER, GET_ITER):
                loop_type = 'for'
            else:
                loop_type = 'while'
                (line_no, next_line_byte) = self.lines[pos]
                test = self.prev[next_line_byte]
                assert(test is not None)
                test_target = self.__get_target(code, test)
                if test_target > (jump_back+3):
                    jump_back = test_target
                 
            self.__loops.append(target)
            self.__structs.append({'type': loop_type + '-loop',
                                   'start': target,
                                   'end':   jump_back})
            self.__structs.append({'type': loop_type + '-else',
                                   'start': jump_back+3,
                                   'end':   end})
        elif op == SETUP_EXCEPT:
            start  = pos+3
            target = self.__get_target(code, pos, op)
            end    = self.__restrict_to_parent(target, parent)
            if target != end:
                self.__fixed_jumps[pos] = end
                #print target, end, parent
            ## Add the try block
            self.__structs.append({'type':  'try',
                                   'start': start,
                                   'end':   end-4})
            ## Now isolate the except and else blocks
            start  = end
            target = self.__get_target(code, self.prev[start])
            self.__fix_parent(code, target, parent)
            end    = self.__restrict_to_parent(target, parent)
            #if target != end:
            #    self.__fixed_jumps[self.prev[start]] = end

            end_finally = self.__last_instr(code, start, end, END_FINALLY)
            if end_finally is None:
                return
            lookup = [JUMP_FORWARD]
            jump_end = self.__last_instr(code, start, end, lookup)
            if jump_end:
                target = self.__get_target(code, jump_end)
                end = self.__restrict_to_parent(target, parent)
            #    if target != end:
            #        self.__fixed_jumps[jump_end] = end
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
       #         if target != end:
       #             self.__fixed_jumps[jmp] = end
                i = jmp+3

        elif op in (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE):
            start = pos+3 
            target = self.__get_target(code, pos, op)
            rtarget = self.__restrict_to_parent(target, parent)
            
            (line_no, next_line_byte) = self.lines[pos]
            
            if target == rtarget:
                prev_target = self.prev[target]
                prev_target_op = ord(code[prev_target])
                target_op = ord(code[target])
                if prev_target_op == JUMP_ABSOLUTE and target_op != POP_BLOCK:
                    if self.__get_target(code, prev_target) < pos:
                        self.__jump_back_else[prev_target] = True

            #is this part of a larger expression
            if (ord(code[self.prev[target]]) in (JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP,
                    POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE)) and (target > pos):  
                self.__fixed_jumps[pos] = self.prev[target]
                return
                
            #is this not at the end of a line
            if line_no == self.lines[start][0]:
                #is this a one line if with multiple tests
                good_op = False
                prev = self.prev[next_line_byte]
                p_op = ord(code[prev])
                if op == POP_JUMP_IF_FALSE:
                    if target == next_line_byte:
                        if p_op == JUMP_FORWARD:
                            if self.__get_target(code, prev) == target:
                                good_op = True
                        if p_op == RETURN_VALUE:
                            good_op = True
                    else:
                        if start < target < next_line_byte:
                            if ord(code[self.prev[target]]) in (JUMP_ABSOLUTE, JUMP_FORWARD, RETURN_VALUE):
                                good_op = True
                        while p_op in (JUMP_ABSOLUTE, JUMP_FORWARD, POP_BLOCK):
                            if p_op in (JUMP_ABSOLUTE, JUMP_FORWARD):
                                if self.__get_target(code, prev) == target:
                                    good_op = True
                                    break
                            prev = self.prev[prev]
                            p_op = ord(code[prev])
                    if good_op:
                        last = self.__last_instr(code, start, next_line_byte,
                                (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE), target)
                        if last:
                            self.__fixed_jumps[pos] = last
                            return
                else:
                    while p_op in (JUMP_ABSOLUTE, JUMP_FORWARD, POP_BLOCK):
                        if p_op in (JUMP_ABSOLUTE, JUMP_FORWARD):
                            if self.__get_target(code, prev) == target:
                                last = self.__last_instr(code, start, next_line_byte,
                                        (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE))
                                if last:
                                    self.__fixed_jumps[pos] = last
                                    return
                                break
                        prev = self.prev[prev]
                        p_op = ord(code[prev])
                 
                #if ifline
                if self.if_lines.get(line_no, False):
                    if (target >= next_line_byte) or (target < pos):
                        if not (line_no == self.lines[target][0]):
                            self.__fixed_jumps[pos] = self.prev[next_line_byte]
                    return
                if self.if_lines.get(line_no+1, False):
                    next_if = self.prev[self.lines[next_line_byte][1]]
                    if target == self.__get_target(code, next_if):
                        self.__fixed_jumps[pos] = next_if
                    elif (op == POP_JUMP_IF_TRUE) and (ord(code[next_if+3]) == JUMP_ABSOLUTE) and (target == self.__get_target(code, next_if+3)) and (target < pos):
                        self.__fixed_jumps[pos] = next_if
                    return
                else:
                    if self.lines[target][0] > line_no:
                        next = self.__first_instr(code, start, target, POP_JUMP_IF_FALSE, target)
                        j = self.__first_instr(code, start, target, JUMP_ABSOLUTE, target)
                        if next and not j:
                            self.__fixed_jumps[pos] = next
                    return
                return
                    
            if op == POP_JUMP_IF_FALSE:
                i = self.lines[next_line_byte][0]
                k = j = next_line_byte
                num_pj = 1
                while ((self.if_lines.get(i, False)
                        and ((self.__get_target(code, self.lines[j][1]-3) == target)
                             or ((ord(code[self.lines[j][1]-3]) == POP_JUMP_IF_TRUE)
                                 and (ord(code[self.__get_target(code, self.lines[j][1]-3)-3]) == POP_JUMP_IF_FALSE)
                                 and (self.__get_target(code, self.__get_target(code, self.lines[j][1]-3)-3) == target))))
                       or (ord(code[self.prev[self.lines[j][1]]]) in (LOAD_ATTR, LOAD_FAST, JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP))):
                    if (self.if_lines.get(i, False) and (self.__get_target(code, self.lines[j][1]-3) == target)):
                        num_pj += 1
                    j = self.lines[j][1]
                    i = self.lines[j][0]
                    if (ord(code[self.prev[j]]) not in (LOAD_ATTR, LOAD_FAST, JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP)):
                        k = j
                if k > next_line_byte:
                    if num_pj > 1 and target > pos:
                        prev_end = self.prev[rtarget]
                        num_pj += len({ self.lines[a][0] for a in self.__all_instr(code, k, prev_end, (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE), target)})
                        num_pr = len({ self.lines[a][0] for a in self.__all_instr(code, k, prev_end, (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE), rtarget)})
                        num_jumps = 0
                        while ord(code[prev_end]) in (JUMP_FORWARD, JUMP_ABSOLUTE) and self.__get_target(code, prev_end) == target:
                            num_pr += len({ self.lines[a][0] for a in self.__all_instr(code, k, prev_end, (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE), prev_end)})
                            num_jumps += 1
                            prev_end = self.prev[prev_end]
                        if ord(code[prev_end]) == RETURN_VALUE:
                            num_jumps += 1
                            num_pj += num_pr
                        num_pj += len(self.__all_instr(code, k, prev_end, (POP_JUMP_IF_FALSE, POP_JUMP_IF_TRUE), target))
                        if num_pj > num_jumps:
                            self.__fixed_jumps[pos] = k-3
                            return
                    else:
                        self.__fixed_jumps[pos] = k-3
                        return
                
#            elif op == POP_JUMP_IF_TRUE and target > pos:
#                i = self.lines[next_line_byte][0]
#                j = next_line_byte
#                while (self.if_lines.get(i, False)
#                       and ((self.__get_target(code, self.lines[j][1]-3) == target)
#                            and (ord(code[self.lines[j][1]-3]) == POP_JUMP_IF_TRUE))):
#                       j = self.lines[j][1]
#                       i = self.lines[j][0]
#                if j > next_line_byte:
#                    self.__fixed_jumps[pos] = j-3
#                    return
            elif op == POP_JUMP_IF_TRUE:
                def equaljumps(jump1, jump2):
                    jump_ops = (JUMP_ABSOLUTE, JUMP_FORWARD)
                    while ord(code[jump1]) in jump_ops:
                        jump1 = self.__get_target(code, jump1)
                    while ord(code[jump2]) in jump_ops:
                        jump2 = self.__get_target(code, jump2)
                    return jump1 == jump2
                i = self.lines[next_line_byte][0]
                j = next_line_byte
                while self.if_lines.get(i, False):
                       j = self.lines[j][1]
                       i = self.lines[j][0]
                if j > next_line_byte:
                    if ord(code[j]) == JUMP_ABSOLUTE and equaljumps(j, target):
                        self.__fixed_jumps[pos] = j-3
                        return
                
            if (target < pos) and ((ord(code[target]) == FOR_ITER) or (ord(code[self.prev[target]]) == SETUP_LOOP)):
#                self.__end_if_line[start] = 0
                
                if ord(code[self.prev[end]]) == JUMP_ABSOLUTE:
                    if self.__get_target(code, self.prev[end]) == target:
                        self.__structs.append({'type':  'if-then',
                                       'start': pos,
                                       'end':   self.prev[end]})
#                        print self.__structs[-1]
                return

            #does the if jump just beyond a jump op, then this is probably an if statement
            if ord(code[self.prev[rtarget]]) in (JUMP_ABSOLUTE, JUMP_FORWARD):
                if_end = self.__get_target(code, self.prev[rtarget])

                if (if_end < self.prev[rtarget]) and (ord(code[self.prev[if_end]]) == SETUP_LOOP):
                    loopjump = self.__last_instr(code, start, end, JUMP_ABSOLUTE, if_end)
                    if(if_end > start):
                        return
                        
                end = self.__restrict_to_parent(if_end, parent)
                                                       
#                self.__end_if_line[start] = rtarget

                self.__structs.append({'type':  'if-then',
                                       'start': start,
                                       'end':   self.prev[rtarget]})

                if rtarget < end:
                    self.__structs.append({'type':  'if-else',
                                       'start': rtarget,
                                       'end':   end})
            elif ord(code[self.prev[rtarget]]) == RETURN_VALUE:
#                self.__end_if_line[start] = rtarget
 #               self.__fixed_jumps[pos] = rtarget
                self.__structs.append({'type':  'if-then',
                                       'start': start,
                                       'end':   rtarget})
        elif op in (JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP):
            target = self.__get_target(code, pos, op)
            if target > pos:
                unop_target = self.__last_instr(code, pos, target, JUMP_FORWARD, target)
                if unop_target and ord(code[unop_target+3]) != ROT_TWO:
                    self.__fixed_jumps[pos] = unop_target
                
                
             

    def find_jump_targets(self, code):
        """
        Detect all offsets in a byte code which are jump targets.

        Return the list of offsets.

        This procedure is modelled after dis.findlables(), but here
        for each target the number of jumps are counted.
        """
        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

        hasjrel = dis.hasjrel
        hasjabs = dis.hasjabs

        needFixing = (self.__pyversion >= 2.3)

        n = len(code)
        self.__structs = [{'type':  'root',
                           'start': 0,
                           'end':   n-1}]
        self.__loops = []  ## All loop entry points
        self.__fixed_jumps = {} ## Map fixed jumps to their real destination
        self.__jump_back_else = {}

        targets = {}
        i = 0
        while i < n:
            op = ord(code[i])

            if needFixing:
                ## Determine structures and fix jumps for 2.3+
                self.__detect_structure(code, i, op)

            if op >= HAVE_ARGUMENT:
                label = self.__fixed_jumps.get(i)
                oparg = ord(code[i+1]) + ord(code[i+2]) * 256
                    
                
                if label is None:
                    if op in hasjrel and op != FOR_ITER:
                        label = i + 3 + oparg
                    elif op in hasjabs:
                        if op in [JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP]:
                            if (oparg > i):
                                label = oparg
                       
                if label is not None:
                    targets[label] = targets.get(label, []) + [i]
                i += 3
            else:
                i += 1
        return targets


__scanners = {}

def getscanner(version):
    if not __scanners.has_key(version):
        __scanners[version] = Scanner(version)
    return __scanners[version]
