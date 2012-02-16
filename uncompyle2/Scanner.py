#  Copyright (c) 1999 John Aycock
#  Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#  Copyright (c) 2005 by Dan Pascu <dan@windowmaker.org>
#
#  See main module for license.
#

__all__ = ['Token', 'Scanner', 'getscanner']

import types
import dis
from collections import namedtuple
from array import array

globals().update({k.replace('+','_'):v for (k,v) in dis.opmap.items()})

PJIF = POP_JUMP_IF_FALSE
PJIT = POP_JUMP_IF_TRUE
JA = JUMP_ABSOLUTE
JF = JUMP_FORWARD

class Token:
    """
    Class representing a byte-code token.
    
    A byte-code token is equivalent to the contents of one line
    as output by dis.dis().
    """
    def __init__(self, type_, attr=None, pattr=None, offset=-1, linestart=False):
        self.type = intern(type_)
        self.attr = attr
        self.pattr = pattr
        self.offset = offset
        self.linestart = linestart
        
    def __cmp__(self, o):
        if isinstance(o, Token):
            # both are tokens: compare type and pattr 
            return cmp(self.type, o.type) or cmp(self.pattr, o.pattr)
        else:
            return cmp(self.type, o)

    def __repr__(self):		return str(self.type)
    def __str__(self):
        pattr = self.pattr or ''
        if self.linestart:
            return '\n%s\t%-17s %r' % (self.offset, self.type, pattr)
        else:
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
        self.version = version

        from sys import version_info
        self.pyversion = float('%d.%d' % version_info[0:2])

        self.resetTokenClass()

        self.JUMP_OPs = map(lambda op: dis.opname[op],
                            dis.hasjrel + dis.hasjabs)        

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
        self.code = code = array('B', co.co_code)
        n = len(code)
        self.prev = [0]
        i=0
        while i < n:
            c = code[i]
            op = code[i]
            if op >= dis.HAVE_ARGUMENT:
                self.prev.append(i)
                self.prev.append(i)
                self.prev.append(i)
                i = i + 3
            else:
                self.prev.append(i)
                i = i + 1
                
        self.lines = []
        linetuple = namedtuple('linetuple', ['l_no', 'next'])
        self.if_lines = {}
        j = 0
        linestarts = list(dis.findlinestarts(co))
        linestartoffsets = {a for (a, _) in linestarts}
        (prev_start_byte, prev_line_no) = linestarts[0]
        for (start_byte, line_no) in linestarts[1:]:
            while j < start_byte:
                self.lines.append(linetuple(prev_line_no, start_byte))
                j += 1
            last_op = code[self.prev[start_byte]]
            if last_op in (PJIF, PJIT):
                self.if_lines[prev_line_no] = True
            else:
                self.if_lines[prev_line_no] = False
            (prev_start_byte, prev_line_no) = (start_byte, line_no)
        while j < n:
            self.lines.append(linetuple(prev_line_no, n))
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
                    
            op = code[i]
            opname = dis.opname[op]
            i += 1
            oparg = None; pattr = None
            if op >= dis.HAVE_ARGUMENT:
                oparg = code[i] + code[i+1] * 256 + extended_arg
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

            if op in (BUILD_LIST, BUILD_TUPLE, BUILD_SET, BUILD_SLICE,
                            UNPACK_SEQUENCE,
                            MAKE_FUNCTION, CALL_FUNCTION, MAKE_CLOSURE,
                            CALL_FUNCTION_VAR, CALL_FUNCTION_KW,
                            CALL_FUNCTION_VAR_KW, DUP_TOPX,
                            ):
                # CE - Hack for >= 2.5
                #      Now all values loaded via LOAD_CLOSURE are packed into
                #      a tuple before calling MAKE_CLOSURE.
                if op == BUILD_TUPLE and \
                    code[offset-3] == LOAD_CLOSURE:
                    continue
                else:
                    opname = '%s_%d' % (opname, oparg)
                    if op != BUILD_SLICE:
                        customize[opname] = oparg
            elif op == JA:
                target = self.get_target(offset)
                if target < offset:
                    opname = 'JUMP_BACK'

            elif op == LOAD_GLOBAL:
                try:
                    if pattr == 'AssertionError' and rv and rv[-1] == 'POP_JUMP_IF_TRUE':
                        opname = 'LOAD_ASSERT'
                except AttributeError:
                    pass

            elif op == IMPORT_NAME:
                if pattr == '':
                    pattr = '.'

            rv.append(Token(opname, oparg, pattr, offset, linestart = offset in linestartoffsets))

            if self.jump_back_else.get(offset, False):
                rv.append(Token('JUMP_BACK_ELSE', None, None, 
                    offset="%s_" % offset ))
            
        if self.showasm:
            out = self.out # shortcut
            for t in rv:
                print >>out, t
            print >>out

        return rv, customize

    def get_target(self, pos, op=None):
        if op is None:
            op = self.code[pos]
        target = self.code[pos+1] + self.code[pos+2] * 256
        if op in dis.hasjrel:
            target += pos + 3
        return target

    def first_instr(self, start, end, instr, target=None, exact=True):
        """
        Find the first <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely if exact
        is True, or if exact is False, the instruction which has a target
        closest to <target> will be returned.

        Return index to it or None if not found.
        """
        code = self.code
        assert(start>=0 and end<=len(code))

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

        try:    None in instr
        except: instr = [instr]

        pos = None
        distance = len(code)
        i = start
        while i < end:
            op = code[i]
            if op in instr:
                if target is None:
                    return i
                dest = self.get_target(i, op)
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

    def last_instr(self, start, end, instr, target=None, exact=True):
        """
        Find the last <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely if exact
        is True, or if exact is False, the instruction which has a target
        closest to <target> will be returned.

        Return index to it or None if not found.
        """

        code = self.code
        if not (start>=0 and end<=len(code)):
            return None

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

        try:    None in instr
        except: instr = [instr]

        pos = None
        distance = len(code)
        i = start
        while i < end:
            op = code[i]
            if op in instr:
                if target is None:
                    pos = i
                else:
                    dest = self.get_target(i, op)
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

    def all_instr(self, start, end, instr, target=None, include_beyond_target=False):
        """
        Find all <instr> in the block from start to end.
        <instr> is any python bytecode instruction or a list of opcodes
        If <instr> is an opcode with a target (like a jump), a target
        destination can be specified which must match precisely.

        Return a list with indexes to them or [] if none found.
        """
        
        code = self.code
        assert(start>=0 and end<=len(code))

        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

        try:    None in instr
        except: instr = [instr]

        result = []
        i = start
        while i < end:
            op = code[i]
            if op in instr:
                if target is None:
                    result.append(i)
                else:
                    t = self.get_target(i, op)
                    if include_beyond_target and t >= target:
                        result.append(i)
                    elif t == target:
                        result.append(i)
            i += self.op_size(op)
        return result

    def op_size(self, op):
        if op < dis.HAVE_ARGUMENT:
            return 1
        else:
            return 3

    def build_stmt_indices(self):
        code = self.code
        start = 0;
        end = len(code)
        
        stmt_opcodes = {
            SETUP_LOOP, BREAK_LOOP, CONTINUE_LOOP,
            SETUP_FINALLY, END_FINALLY, SETUP_EXCEPT, SETUP_WITH,
            POP_BLOCK, STORE_FAST, DELETE_FAST, STORE_DEREF,
            STORE_GLOBAL, DELETE_GLOBAL, STORE_NAME, DELETE_NAME,
            STORE_ATTR, DELETE_ATTR, STORE_SUBSCR, DELETE_SUBSCR,
            IMPORT_NAME, IMPORT_FROM, RETURN_VALUE, RAISE_VARARGS, POP_TOP,
            PRINT_EXPR, PRINT_ITEM, PRINT_NEWLINE, PRINT_ITEM_TO, PRINT_NEWLINE_TO,
            STORE_SLICE_0, STORE_SLICE_1, STORE_SLICE_2, STORE_SLICE_3,
            DELETE_SLICE_0, DELETE_SLICE_1, DELETE_SLICE_2, DELETE_SLICE_3,
            JUMP_ABSOLUTE,
        }

        stmt_opcode_seqs = [(PJIF, JF), (PJIF, JA), (PJIT, JF), (PJIT, JA)]
        
        designator_ops = {
            STORE_FAST, STORE_NAME, STORE_GLOBAL, STORE_DEREF, STORE_ATTR, 
            STORE_SLICE_0, STORE_SLICE_1, STORE_SLICE_2, STORE_SLICE_3,
            STORE_SUBSCR, UNPACK_SEQUENCE,
        }

        prelim = self.all_instr(start, end, stmt_opcodes)

        stmts = self.stmts = set(prelim)
                
        pass_stmts = set()
        for seq in stmt_opcode_seqs:
            i = start
            while i+len(seq)-1 < end:
                match = True
                j = i
                for elem in seq:
                    if elem != code[j]:
                        match = False
                        break
                    j += self.op_size(code[j])
                    
                if match:
                    j = self.prev[j]
                    stmts.add(j)
                    pass_stmts.add(j)
                i += self.op_size(code[i])
        
        if pass_stmts:
            stmt_list = list(stmts)
            stmt_list.sort()
        else:
            stmt_list = prelim
        last_stmt = -1
        self.next_stmt = []
        slist = self.next_stmt = []
        i = 0
        for s in stmt_list:
            if code[s] == JA and s not in pass_stmts:
                target = self.get_target(s)
                if target > s or self.lines[last_stmt].l_no == self.lines[s].l_no:
                    stmts.remove(s)
                    continue
                j = self.prev[s]
                while j == JA:
                    j = self.prev[j]
                if code[j] == LIST_APPEND: #list comprehension
                    stmts.remove(s)
                    continue
            elif code[s] == POP_TOP and code[self.prev[s]] == ROT_TWO:
                stmts.remove(s)
                continue
            elif code[s] in designator_ops:
                j = self.prev[s]
                while code[j] in designator_ops:
                    j = self.prev[j]
                if code[j] == FOR_ITER:
                    stmts.remove(s)
                    continue
            last_stmt_line = self.lines[s].l_no
            slist += [s] * (s-i)
            i = s
        slist += [len(code)] * (len(code)-len(slist))
                    
               
    def remove_mid_line_ifs(self, ifs):
        filtered = []
        for i in ifs:
            if self.lines[i].l_no == self.lines[i+3].l_no:
                if self.code[self.prev[self.lines[i].next]] in (PJIT, PJIF):
                    continue
            filtered.append(i)
        return filtered

    
    def next_except_jump(self, start, end, target):
        """
        Return the next jump that was generated by an except SomeException:
        construct in a try...except...else clause or None if not found.
        """
        HAVE_ARGUMENT = dis.HAVE_ARGUMENT

        inner_try = self.first_instr(start, end, SETUP_EXCEPT, end, False)

        lookup = [JA, JF]
        while start < end:
            jmp = self.first_instr(start, end, lookup, target)
            if jmp is None:
                return None
            if jmp == self.prev[end]:
                return jmp
            after = jmp + 3
            ops = [None, None, None, None]
            opp = [0, 0, 0, 0]
            pos = 0
            x = jmp+3
            while x <= end and pos < 4:
                op = self.code[x]
                if op >= HAVE_ARGUMENT:
                    break
                ops[pos] = op
                opp[pos] = x
                pos += 1
                x += 1
            if (ops[0] == END_FINALLY and opp[0] == end)\
                  or (ops[0] == DUP_TOP)\
                  or (ops[0] == ops[1] == ops[2] == POP_TOP):
                inner_trys = self.all_instr(start, jmp, SETUP_EXCEPT)
                inner_finallys = self.all_instr(start, jmp, END_FINALLY)
                if len(inner_trys) == len(inner_finallys):
                    return jmp
            start = jmp + 3
        return None

    def fix_parent(self, target, parent):
        """Fix parent boundaries if needed"""
        start = parent['start']
        end = parent['end']

        if target >= start or end-start < 3 or target not in self.loops:
            return
        if self.code[self.prev[end]]==JA:
            cont_target = self.get_target(end-3, JA)
            if target == cont_target:
                parent['end'] = end-3

    def restrict_to_parent(self, target, parent):
        """Restrict pos to parent boundaries."""
        if not (parent['start'] < target < parent['end']):
            target = parent['end']
        return target

        
    def detect_structure(self, pos, op=None):
        """
        Detect structures and their boundaries to fix optimizied jumps
        in python2.3+
        """

        # TODO: check the struct boundaries more precisely -Dan

        code = self.code
        # Ev remove this test and make op a mandatory argument -Dan
        if op is None:
            op = code[pos]

        ## Detect parent structure
        parent = self.structs[0]
        start  = parent['start']
        end    = parent['end']
        for s in self.structs:
            _start = s['start']
            _end   = s['end']
            if (_start <= pos < _end) and (_start >= start and _end <= end):
                start  = _start
                end    = _end
                parent = s

        ## We need to know how many new structures were added in this run
        origStructCount = len(self.structs)

        if op == SETUP_LOOP:
            start = pos+3
            target = self.get_target(pos, op)
            end    = self.restrict_to_parent(target, parent)
            if target != end:
                self.fixed_jumps[pos] = end
            
            (line_no, next_line_byte) = self.lines[pos]
            jump_back = self.last_instr(start, end, JA,
                                          next_line_byte, False)
            if not jump_back:
                return

            if self.get_target(jump_back) >= next_line_byte:
                jump_back = self.last_instr(start, end, JA,
                                          start, False)

                
            if end > jump_back+4 and code[end] in (JF, JA):
                if code[jump_back+4] in (JA, JF):
                    if self.get_target(jump_back+4) == self.get_target(end):
                        self.fixed_jumps[pos] = jump_back+4
                        end = jump_back+4
            elif target < pos:
                self.fixed_jumps[pos] = jump_back+4
                end = jump_back+4
             
            target = self.get_target(jump_back, JA)

            if code[target] in (FOR_ITER, GET_ITER):
                loop_type = 'for'
            else:
                loop_type = 'while'
                (line_no, next_line_byte) = self.lines[pos]
                test = self.prev[next_line_byte]
                assert(test is not None)
                test_target = self.get_target(test)
                if test_target > (jump_back+3):
                    jump_back = test_target
                 
            self.loops.append(target)
            self.structs.append({'type': loop_type + '-loop',
                                   'start': target,
                                   'end':   jump_back})
            self.structs.append({'type': loop_type + '-else',
                                   'start': jump_back+3,
                                   'end':   end})
        elif op == SETUP_EXCEPT:
            start  = pos+3
            target = self.get_target(pos, op)
            end    = self.restrict_to_parent(target, parent)
            if target != end:
                self.fixed_jumps[pos] = end
                #print target, end, parent
            ## Add the try block
            self.structs.append({'type':  'try',
                                   'start': start,
                                   'end':   end-4})
            ## Now isolate the except and else blocks
            start  = end
            target = self.get_target(self.prev[start])
            self.fix_parent(target, parent)
            end    = self.restrict_to_parent(target, parent)
            #if target != end:
            #    self.fixed_jumps[self.prev[start]] = end

            end_finally = self.last_instr(start, end, END_FINALLY)
            if end_finally is None:
                return
            lookup = [JF]
            jump_end = self.last_instr(start, end, lookup)
            if jump_end:
                target = self.get_target(jump_end)
                end = self.restrict_to_parent(target, parent)
            #    if target != end:
            #        self.fixed_jumps[jump_end] = end
           ## Add the try-else block
            self.structs.append({'type':  'try-else',
                                   'start': end_finally+1,
                                   'end':   end})
            ## Add the except blocks
            i = start
            while i < end_finally:
                jmp = self.next_except_jump(i, end_finally, target)
                if jmp is None:
                    break
                self.structs.append({'type':  'except',
                                       'start': i,
                                       'end':   jmp})
       #         if target != end:
       #             self.fixed_jumps[jmp] = end
                i = jmp+3

        elif op in (PJIF, PJIT):
            
            start = pos+3 
            target = self.get_target(pos, op)
            rtarget = self.restrict_to_parent(target, parent)
            pre = self.prev
            
            #does this jump to right after another cond jump?
            # if so, it's part of a larger conditional
            if (code[pre[target]] in (JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP,
                    PJIF, PJIT)) and (target > pos):  
                self.fixed_jumps[pos] = pre[target]
                return

            # is this an if-else at end of a loop?
            # if so, indicate with special opcode to help parser
            if target == rtarget:
                if code[pre[target]] == JA and code[target] != POP_BLOCK:
                    if self.get_target(pre[target]) < pos:
                        self.jump_back_else[pre[target]] = True
            
            # is this an if and
            if op == PJIF:
                #import pdb; pdb.set_trace()
                match = self.all_instr(start, self.next_stmt[pos], (PJIF, PJIT), target)
                match = self.remove_mid_line_ifs(match)
                if match:
                    if code[pre[rtarget]] in (JF, JA) \
                            and pre[rtarget] not in self.stmts \
                            and self.restrict_to_parent(self.get_target(pre[rtarget]), parent) == rtarget:
                        if code[pre[pre[rtarget]]] == JA \
                                and target == self.get_target(pre[pre[rtarget]]) \
                                and pre[pre[rtarget]] not in self.stmts \
                                and 1 == len(self.remove_mid_line_ifs(self.all_instr(start, pre[pre[rtarget]], \
                                                            (PJIF, PJIT), target))):
                            pass
                        elif code[pre[pre[rtarget]]] == RETURN_VALUE \
                                and 1 == (len(set(self.remove_mid_line_ifs(self.all_instr(start, pre[pre[rtarget]], \
                                                             (PJIF, PJIT), target))) \
                                          | set(self.remove_mid_line_ifs(self.all_instr(start, pre[pre[rtarget]], \
                                                           (PJIF, PJIT, JA), pre[rtarget], True))))):
                            pass
                        else:
                            fix = None
                            jump_ifs = self.all_instr(start, self.next_stmt[pos], (PJIF, PJIT))
                            last_jump_good = True
                            for j in jump_ifs:
                                if code[j] == PJIF and target == self.get_target(j):
                                    if self.lines[j].next == j+3 and last_jump_good:
                                        fix = j
                                        break
                                else:
                                    last_jump_good = False
                            self.fixed_jumps[pos] = fix or match[-1]
                            return
                    else:
                        self.fixed_jumps[pos] = match[-1]
                        return
            else:
                next = self.next_stmt[pos]
                if pre[next] == pos:
                    pass
                elif code[next] in (JF, JA) and target == self.get_target(next):
                    if code[pre[next]] in (PJIF, PJIT):
                        self.fixed_jumps[pos] = pre[next]
                        return
                elif code[next] == JA and code[target] in (JA, JF) \
                      and self.get_target(target) == self.get_target(next):
                    self.fixed_jumps[pos] = pre[next]
                    return
                        
            #does the if jump just beyond a jump op, then this is probably an if statement
            if code[pre[rtarget]] in (JA, JF):
                if_end = self.get_target(pre[rtarget])
                
                #is this a loop not an if?
                if (if_end < pre[rtarget]) and (code[pre[if_end]] == SETUP_LOOP):
                    if(if_end > start):
                        return
                        
                end = self.restrict_to_parent(if_end, parent)
                                                       
                self.structs.append({'type':  'if-then',
                                       'start': start,
                                       'end':   pre[rtarget]})

                if rtarget < end:
                    self.structs.append({'type':  'if-else',
                                       'start': rtarget,
                                       'end':   end})
            elif code[pre[rtarget]] == RETURN_VALUE:
                self.structs.append({'type':  'if-then',
                                       'start': start,
                                       'end':   rtarget})

        elif op in (JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP):
            target = self.get_target(pos, op)
            if target > pos:
                unop_target = self.last_instr(pos, target, JF, target)
                if unop_target and code[unop_target+3] != ROT_TWO:
                    self.fixed_jumps[pos] = unop_target
                
                
             

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

        n = len(code)
        self.structs = [{'type':  'root',
                           'start': 0,
                           'end':   n-1}]
        self.loops = []  ## All loop entry points
        self.fixed_jumps = {} ## Map fixed jumps to their real destination
        self.jump_back_else = {}
        self.build_stmt_indices()

        targets = {}
        i = 0
        while i < n:
            op = code[i]

            ## Determine structures and fix jumps for 2.3+
            self.detect_structure(i, op)

            if op >= HAVE_ARGUMENT:
                label = self.fixed_jumps.get(i)
                oparg = code[i+1] + code[i+2] * 256
                    
                
                if label is None:
                    if op in hasjrel and op != FOR_ITER:
                        label = i + 3 + oparg
                    elif op in hasjabs:
                        if op in (JUMP_IF_FALSE_OR_POP, JUMP_IF_TRUE_OR_POP):
                            if (oparg > i):
                                label = oparg
                       
                if label is not None:
                    targets[label] = targets.get(label, []) + [i]
            i += self.op_size(op)
        return targets


__scanners = {}

def getscanner(version):
    if not __scanners.has_key(version):
        __scanners[version] = Scanner(version)
    return __scanners[version]
