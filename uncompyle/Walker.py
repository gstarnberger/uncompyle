#  Copyright (c) 1999 John Aycock
#  Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#  Copyright (c) 2005 by Dan Pascu <dan@windowmaker.org>
#
#  See main module for license.
#
#
#  Decompilation (walking AST)
#
#  All table-driven.  Step 1 determines a table (T) and a path to a
#  table key (K) from the node type (N) (other nodes are shown as O):
#
#         N                  N               N&K
#     / | ... \          / | ... \        / | ... \
#    O  O      O        O  O      K      O  O      O
#              |
#              K
#
#  MAP_R0 (TABLE_R0)  MAP_R (TABLE_R)  MAP_DIRECT (TABLE_DIRECT)
#
#  The default is a direct mapping.  The key K is then extracted from the
#  subtree and used to find a table entry T[K], if any.  The result is a
#  format string and arguments (a la printf()) for the formatting engine.
#  Escapes in the format string are:
#
#	%c	evaluate N[A] recursively*
#	%C	evaluate N[A[0]]..N[A[1]] recursively, separate by A[2]*
#	%,	print ',' if last %C only printed one item (for tuples--unused)
#	%|	tab to current indentation level
#	%+	increase current indentation level
#	%-	decrease current indentation level
#	%{...}	evaluate ... in context of N
#	%%	literal '%'
#
#  * indicates an argument (A) required.
#
#  The '%' may optionally be followed by a number (C) in square brackets, which
#  makes the engine walk down to N[C] before evaluating the escape code.
#

import sys, re, cStringIO
from types import ListType, TupleType, DictType, \
     EllipsisType, IntType, CodeType

from spark import GenericASTTraversal
import Parser
from Parser import AST
from Scanner import Token, Code

minint = -sys.maxint-1

# Some ASTs used for comparing code fragments (like 'return None' at
# the end of functions).

RETURN_LOCALS = AST('stmt',
		    [ AST('return_stmt',
			  [ AST('expr', [ Token('LOAD_LOCALS') ]),
			    Token('RETURN_VALUE')]) ])


NONE = AST('expr', [ Token('LOAD_CONST', pattr=None) ] )

RETURN_NONE = AST('stmt',
		  [ AST('return_stmt',
			[ NONE, Token('RETURN_VALUE')]) ])

ASSIGN_DOC_STRING = lambda doc_string: \
	AST('stmt',
	    [ AST('assign',
		  [ AST('expr', [ Token('LOAD_CONST', pattr=doc_string) ]),
		    AST('designator', [ Token('STORE_NAME', pattr='__doc__')])
		    ])])

BUILD_TUPLE_0 = AST('expr',
                    [ AST('build_list',
                          [ Token('BUILD_TUPLE_0') ])])

#TAB = '\t'			# as God intended
TAB = ' ' *4   # is less spacy than "\t"
INDENT_PER_LEVEL = ' ' # additional intent per pretty-print level

TABLE_R = {
    'build_tuple2':	( '%C', (0,-1,', ') ),
    'POP_TOP':		( '%|%c\n', 0 ),
    'STORE_ATTR':	( '%c.%[1]{pattr}', 0),
#   'STORE_SUBSCR':	( '%c[%c]', 0, 1 ),
    'STORE_SLICE+0':	( '%c[:]', 0 ),
    'STORE_SLICE+1':	( '%c[%c:]', 0, 1 ),
    'STORE_SLICE+2':	( '%c[:%c]', 0, 1 ),
    'STORE_SLICE+3':	( '%c[%c:%c]', 0, 1, 2 ),
    'JUMP_ABSOLUTE':	( '%|continue\n', ),
    'DELETE_SLICE+0':	( '%|del %c[:]\n', 0 ),
    'DELETE_SLICE+1':	( '%|del %c[%c:]\n', 0, 1 ),
    'DELETE_SLICE+2':	( '%|del %c[:%c]\n', 0, 1 ),
    'DELETE_SLICE+3':	( '%|del %c[%c:%c]\n', 0, 1, 2 ),
    'DELETE_ATTR':	( '%|del %c.%[-1]{pattr}\n', 0 ),
#   'EXEC_STMT':	( '%|exec %c in %[1]C\n', 0, (0,sys.maxint,', ') ),
    'BINARY_SUBSCR':	( '%c[%c]', 0, 1), # required for augmented assign
    'UNARY_POSITIVE':	( '+%c', 0 ),
    'UNARY_NEGATIVE':	( '-%c', 0 ),
    'UNARY_CONVERT':	( '`%c`', 0 ),
    'UNARY_INVERT':	( '~%c', 0 ),
    'UNARY_NOT':	( '(not %c)', 0 ),
    'SLICE+0':		( '%c[:]', 0 ),
    'SLICE+1':		( '%c[%c:]', 0, 1 ),
    'SLICE+2':		( '%c[:%c]', 0, 1 ),
    'SLICE+3':		( '%c[%c:%c]', 0, 1, 2 ),
}
TABLE_R0 = {
#    'BUILD_LIST':	( '[%C]',      (0,-1,', ') ),
#    'BUILD_TUPLE':	( '(%C)',      (0,-1,', ') ),
#    'CALL_FUNCTION':	( '%c(%C)', 0, (1,-1,', ') ),
}
TABLE_DIRECT = {
    'BINARY_ADD':	( '+' ,),
    'BINARY_SUBTRACT':	( '-' ,),
    'BINARY_MULTIPLY':	( '*' ,),
    'BINARY_DIVIDE':	( '/' ,),
    'BINARY_TRUE_DIVIDE':	( '/' ,),
    'BINARY_FLOOR_DIVIDE':	( '//' ,),
    'BINARY_MODULO':	( '%%',),
    'BINARY_POWER':	( '**',),
    'BINARY_LSHIFT':	( '<<',),
    'BINARY_RSHIFT':	( '>>',),
    'BINARY_AND':	( '&' ,),
    'BINARY_OR':	( '|' ,),
    'BINARY_XOR':	( '^' ,),
    'INPLACE_ADD':	( '+=' ,),
    'INPLACE_SUBTRACT':	( '-=' ,),
    'INPLACE_MULTIPLY':	( '*=' ,),
    'INPLACE_DIVIDE':	( '/=' ,),
    'INPLACE_TRUE_DIVIDE':	( '/=' ,),
    'INPLACE_FLOOR_DIVIDE':	( '//=' ,),
    'INPLACE_MODULO':	( '%%=',),
    'INPLACE_POWER':	( '**=',),
    'INPLACE_LSHIFT':	( '<<=',),
    'INPLACE_RSHIFT':	( '>>=',),
    'INPLACE_AND':	( '&=' ,),
    'INPLACE_OR':	( '|=' ,),
    'INPLACE_XOR':	( '^=' ,),
    'binary_expr':	( '(%c %c %c)', 0, -1, 1 ),

    'IMPORT_FROM':	( '%{pattr}', ),
    'LOAD_ATTR':	( '.%{pattr}', ),
    'LOAD_FAST':	( '%{pattr}', ),
    'LOAD_NAME':	( '%{pattr}', ),
    'LOAD_GLOBAL':	( '%{pattr}', ),
    'LOAD_DEREF':	( '%{pattr}', ),
    'LOAD_LOCALS':	( 'locals()', ),
#   'LOAD_CONST':	( '%{pattr}', ),	# handled by n_LOAD_CONST
    'DELETE_FAST':	( '%|del %{pattr}\n', ),
    'DELETE_NAME':	( '%|del %{pattr}\n', ),
    'DELETE_GLOBAL':	( '%|del %{pattr}\n', ),
    'delete_subscr':	( '%|del %c[%c]\n', 0, 1,),
    'binary_subscr':	( '%c[%c]', 0, 1),
    'store_subscr':	( '%c[%c]', 0, 1),
    'STORE_FAST':	( '%{pattr}', ),
    'STORE_NAME':	( '%{pattr}', ),
    'STORE_GLOBAL':	( '%{pattr}', ),
    'STORE_DEREF':	( '%{pattr}', ),
    'unpack':		( '(%C,)', (1, sys.maxint, ', ') ),
    'unpack_list':	( '[%C]', (1, sys.maxint, ', ') ),

    #'list_compr':	( '[ %c ]', -2),	# handled by n_list_compr
    'list_iter':	( '%c', 0),
    'list_for':		( ' for %c in %c%c', 2, 0, 3 ),
    'list_if':		( ' if %c%c', 0, 2 ),
    'lc_body':		( '', ),	# ignore when recusing
    
    'assign':		( '%|%c = %c\n', -1, 0 ),
    'augassign1':	( '%|%c %c %c\n', 0, 2, 1),
    'augassign2':	( '%|%c%c %c %c\n', 0, 2, -3, -4),
#   'dup_topx':		( '%c', 0),
    'designList':	( '%c = %c', 0, -1 ),
    'and':          	( '(%c and %c)', 0, 3 ),
    'and2':          	( '%c', 4 ),
    'or':           	( '(%c or %c)', 0, 3 ),
    'compare':		( '(%c %[-1]{pattr} %c)', 0, 1 ),
    'cmp_list':		( '%c %c', 0, 1),
    'cmp_list1':	( '%[3]{pattr} %c %c', 0, -2),
    'cmp_list2':	( '%[1]{pattr} %c', 0),
#   'classdef': 	(), # handled by n_classdef()
    'funcdef':  	( '\n%|def %c\n', -2), # -2 to handle closures
    'kwarg':    	( '%[0]{pattr}=%c', 1),
    'importstmt':	( '%|import %[0]{pattr}\n', ),
    'importfrom':	( '%|from %[0]{pattr} import %c\n', 1 ),
    'importlist':	( '%C', (0, sys.maxint, ', ') ),
    'importstmt2':	( '%|import %c\n', 1),
    'importstar2':	( '%|from %[1]{pattr} import *\n', ),
    'importfrom2':	( '%|from %[1]{pattr} import %c\n', 2 ),
    'importlist2':	( '%C', (0, sys.maxint, ', ') ),
    
    'assert':		( '%|assert %c\n' , 3 ),
    'assert2':		( '%|assert %c, %c\n' , 3, -5 ),
    'assert3':		( '%|assert %c\n' , 0 ),
    'assert4':		( '%|assert %c, %c\n' , 0, -4 ),

    'print_stmt':	( '%|print %c,\n', 0 ),
    'print_stmt_nl':	( '%|print %[0]C\n', (0,1, None) ),
    'print_nl_stmt':	( '%|print\n', ),
    'print_to':		( '%|print >> %c, %c,\n', 0, 1 ),
    'print_to_nl':	( '%|print >> %c, %c\n', 0, 1 ),
    'print_nl_to':	( '%|print >> %c\n', 0 ),
    'print_to_items':	( '%C', (0, 2, ', ') ),

    'call_stmt':	( '%|%c\n', 0),
    'break_stmt':	( '%|break\n', ),
    'continue_stmt':	( '%|continue\n', ),
    'raise_stmt':	( '%|raise %[0]C\n', (0,sys.maxint,', ') ),
    'yield_stmt':	( '%|yield %c\n', 0),
    'return_stmt':	( '%|return %c\n', 0),
    'return_lambda':	( '%c', 0),

    'ifstmt':		( '%|if %c:\n%+%c%-', 0, 2 ),
    'ifelsestmt':	( '%|if %c:\n%+%c%-%|else:\n%+%c%-', 0, 2, -2 ),
    'ifelifstmt':	( '%|if %c:\n%+%c%-%c', 0, 2, -2 ),
    'elifelifstmt':	( '%|elif %c:\n%+%c%-%c', 0, 2, -2 ),
    'elifstmt':		( '%|elif %c:\n%+%c%-', 0, 2 ),
    'elifelsestmt':	( '%|elif %c:\n%+%c%-%|else:\n%+%c%-', 0, 2, -2 ),

    'whilestmt':	( '%|while %c:\n%+%c%-\n', 1, 4 ),
    'while1stmt':	( '%|while 1:\n%+%c%-\n', 5 ),
    'whileelsestmt':	( '%|while %c:\n%+%c%-%|else:\n%+%c%-\n', 1, 4, -2 ),
    'while1elsestmt':	( '%|while 1:\n%+%c%-%|else:\n%+%c%-\n', 5, -2 ),
    'forstmt':		( '%|for %c in %c:\n%+%c%-\n', 3, 1, 4 ),
    'forelsestmt':	(
        '%|for %c in %c:\n%+%c%-%|else:\n%+%c%-\n', 3, 1, 4, -2
     ),
    'trystmt':		( '%|try:\n%+%c%-%c', 1, 5 ),
    'except':		( '%|except:\n%+%c%-', 3 ),
    'except_cond1':	( '%|except %c:\n%+%c%-', 1, 8 ),
    'except_cond2':	( '%|except %c, %c:\n%+%c%-', 1, 6, 8 ),
    'except_else':	( '%|else:\n%+%c%-', 2 ),
    'tryfinallystmt':	( '%|try:\n%+%c%-\n%|finally:\n%+%c%-\n', 1, 5 ),
    'passstmt':		( '%|pass\n', ),
    'STORE_FAST':	( '%{pattr}', ),
    'kv':		( '%c: %c', 3, 1 ),
    'kv2':		( '%c: %c', 1, 2 ),
    'mapexpr':		( '{%[1]C}', (0,sys.maxint,', ') ),
    
    ##
    ## Python 2.5 Additions
    ##
    
    # Import style for 2.5
    '_25_importstmt': ( '%|import %c\n', 2),
    '_25_importstar': ( '%|from %[2]{pattr} import *\n', ),
    '_25_importfrom': ( '%|from %[2]{pattr} import %c\n', 3 ),
    
    # If statement fixes
    # CE - These values should be correct by themselves, the main difference
    #      is is the composition of and/or on the Parser side.
    '_25_and':            ( '(%c and %c)', 0, 3 ),
    '_25_and_end':        ( '%c', 0),
    '_25_or':             ( '(%c or %c)', 0, 3),
    '_25_or_end':         ( '%c', 0),
    
    # CE - These should also be fine as they are.
    '_25_ifstmt':     ( '%|if %c:\n%+%c%-', 0, 1),
    '_25_ifelsestmt': ( '%|if %c:\n%+%c%-%|else:\n%+%c%-', 0, 1, -2),
    '_25_jumptarget_e': ( '%c', -1),
    '_25_jumptarget_s': ( '%c', -1),

    # CE - Fixes for tuples
    '_25_assign2':     ( '(%c, %c,) = (%c, %c)\n', 3, 4, 0, 1 ),
    '_25_assign3':     ( '(%c, %c, %c,) = (%c, %c, %c)\n', 5, 6, 7, 0, 1, 2 ),

}


MAP_DIRECT = (TABLE_DIRECT, )
MAP_R0 = (TABLE_R0, -1, 0)
MAP_R = (TABLE_R, -1)

MAP = {
    'stmt':		MAP_R,
    'del_stmt':		MAP_R,
    'designator':	MAP_R,
    'expr':		MAP_R,
    'exprlist':		MAP_R0,
}


ASSIGN_TUPLE_PARAM = lambda param_name: \
             AST('expr', [ Token('LOAD_FAST', pattr=param_name) ])

escape = re.compile(r'''
            (?P<prefix> [^%]* )
            % ( \[ (?P<child> -? \d+ ) \] )?
                ((?P<type> [^{] ) |
                 ( [{] (?P<expr> [^}]* ) [}] ))
        ''', re.VERBOSE)

class ParserError(Parser.ParserError):
    def __init__(self, error, tokens):
        self.error = error # previous exception
        self.tokens = tokens

    def __str__(self):
        lines = ['--- This code section failed: ---']
        lines.extend( map(str, self.tokens) )
        lines.extend( ['', str(self.error)] )
        return '\n'.join(lines)


__globals_tokens__ =  ('STORE_GLOBAL', 'DELETE_GLOBAL') # 'LOAD_GLOBAL'

def find_globals(node, globals):
    """Find globals in this statement."""
    for n in node:
        if isinstance(n, AST):
            #if n != 'stmt': # skip nested statements
                globals = find_globals(n, globals)
        elif n.type in __globals_tokens__:
            globals[n.pattr] = None
    return globals


class Walker(GenericASTTraversal, object):
    stacked_params = ('f', 'indent', 'isLambda', '_globals')

    def __init__(self, out, scanner, showast=0):
        GenericASTTraversal.__init__(self, ast=None)
        self.scanner = scanner
        params = {
            'f': out,
            'indent': '',
            }
        self.showast = showast
        self.__params = params
        self.__param_stack = []

    f = property(lambda s: s.__params['f'],
                 lambda s, x: s.__params.__setitem__('f', x),
                 lambda s: s.__params.__delitem__('f'),
                 None)

    indent = property(lambda s: s.__params['indent'],
                 lambda s, x: s.__params.__setitem__('indent', x),
                 lambda s: s.__params.__delitem__('indent'),
                 None)

    isLambda = property(lambda s: s.__params['isLambda'],
                 lambda s, x: s.__params.__setitem__('isLambda', x),
                 lambda s: s.__params.__delitem__('isLambda'),
                 None)

    _globals = property(lambda s: s.__params['_globals'],
                 lambda s, x: s.__params.__setitem__('_globals', x),
                 lambda s: s.__params.__delitem__('_globals'),
                 None)

    def indentMore(self, indent=TAB):
        self.indent += indent
    def indentLess(self, indent=TAB):
        self.indent = self.indent[:-len(indent)]

    def traverse(self, node, indent=None, isLambda=0):
        self.__param_stack.append(self.__params)
        if indent is None: indent = self.indent
        self.__params = {
            '_globals': {},
            'f': cStringIO.StringIO(),
            'indent': indent,
            'isLambda': isLambda,
            }
        self.preorder(node)
        result = self.f.getvalue()
        self.__params = self.__param_stack.pop()
        return result
    
    def write(self, *data):
        if type(data) == ListType:
            self.f.writelines(data)
        elif type(data) == TupleType:
            self.f.writelines(list(data))
        else:
            self.f.write(data)

    def print_(self, *data):
        self.write(*data)
        print >> self.f

    def print_docstring(self, indent, docstring):

        def unquote(quote, string):
            unquote = '\\' + quote
            while string.find(quote) >= 0:
                    string = string.replace(quote, unquote)
            return string
        
        if docstring.find('\\\n') or docstring.find('\n'): # multiline string
            if docstring.find('"""') >=0:
                quote = "'''"
            else:
                quote = '"""';
            unquote(quote, docstring)
            docstring = docstring.split('\n')
            self.write(indent, quote)
            for i in range(len(docstring)-1):
                self.print_( repr(docstring[i])[1:-1] )
            self.print_(repr(docstring[-1])[1:-1], quote)
        else:
            self.print_(indent, repr(docstring))
            
    def n_LOAD_CONST(self, node):
        data = node.pattr; datatype = type(data)
        if datatype is IntType and data == minint:
            # convert to hex, since decimal representation
            # would result in 'LOAD_CONST; UNARY_NEGATIVE'
            # change:hG/2002-02-07: this was done for all negative integers
            # todo: check whether this is necessary in Python 2.1
            self.write( hex(data) )
        elif datatype is EllipsisType:
            self.write('...')
        elif data is None:
            # LOAD_CONST 'None' only occurs, when None is
            # implicit eg. in 'return' w/o params
            pass
        else:
            self.write(repr(data))
        # LOAD_CONST is a terminal, so stop processing/recursing early
        self.prune()


    def n_delete_subscr(self, node):
        maybe_tuple = node[-2][-1]
        if maybe_tuple.type.startswith('BUILD_TUPLE'):
            maybe_tuple.type = 'build_tuple2'
        self.default(node)

    n_store_subscr = n_binary_subscr = n_delete_subscr
        

    def n_exec_stmt(self, node):
        """
        exec_stmt ::= expr exprlist DUP_TOP EXEC_STMT
        exec_stmt ::= expr exprlist EXEC_STMT
        """
        self.write(self.indent, 'exec ')
        self.preorder(node[0])
        if node[1][0] != NONE:
            sep = ' in '
            for subnode in node[1]:
                self.write(sep); sep = ", "
                self.preorder(subnode)
        self.print_()
        self.prune() # stop recursing
            
    def n_list_compr(self, node):
        n = node[-2]
        assert n == 'list_iter'
        # find innerst node
        while n == 'list_iter':
            n = n[0] # recurse one step
            if   n == 'list_for':	n = n[3]
            elif n == 'list_if':	n = n[2]
        assert n == 'lc_body'
        self.write( '[ '); 
        self.preorder(n[1]) # lc_body
        self.preorder(node[-2]) # for/if parts
        self.write( ' ]')
        self.prune() # stop recursing
       
    def n_ifelsestmt(self, node, preprocess=0):
        if len(node[-2]) == 1:
            ifnode = node[-2][0][0]
            if ifnode == 'ifelsestmt':
                node.type = 'ifelifstmt'
                self.n_ifelsestmt(ifnode, preprocess=1)
                if ifnode == 'ifelifstmt':
                    ifnode.type = 'elifelifstmt'
                elif ifnode == 'ifelsestmt':
                    ifnode.type = 'elifelsestmt'
            elif ifnode == 'ifstmt':
                node.type = 'ifelifstmt'
                ifnode.type = 'elifstmt'
        if not preprocess:
            self.default(node)

    def n_import_as(self, node):
        iname = node[0].pattr;
        assert node[-1][-1].type.startswith('STORE_')
        sname = node[-1][-1].pattr # assume one of STORE_.... here
        if iname == sname or iname.startswith(sname + '.'):
            self.write(iname)
        else:
            self.write(iname, ' as ', sname)
        self.prune() # stop recursing

    def n_mkfunc(self, node):
        self.write(node[-2].attr.co_name) # = code.co_name
        self.indentMore()
        self.make_function(node, isLambda=0)
        self.indentLess()
        self.prune() # stop recursing

    def n_mklambda(self, node):
        self.make_function(node, isLambda=1)
        self.prune() # stop recursing

    def n_classdef(self, node):
        # class definition ('class X(A,B,C):')
        assert node[0].pattr == node[-1][-1].pattr
        self.write(self.indent, 'class ', str(node[-1][-1].pattr))
        if node[1] != BUILD_TUPLE_0: # this is a subclass
            self.preorder(node[1]) # output superclasses
        self.print_(':')

        # class body
        self.indentMore()
        self.build_class(node[-4][-2].attr)
        self.indentLess()
        self.prune()

    def n_mapexpr(self, node):
        """
        prettyprint a mapexpr
        'mapexpr' is something like k = {'a': 1, 'b': 42 }"
        """
        assert node[-1] == 'kvlist'
        node = node[-1] # goto kvlist

        self.indentMore(INDENT_PER_LEVEL)
        line_seperator = ',\n' + self.indent
        sep = INDENT_PER_LEVEL[:-1]
        self.write('{')
        for kv in node:
            assert kv in ('kv', 'kv2')
            # kv ::= DUP_TOP expr ROT_TWO expr STORE_SUBSCR
            # kv2 ::= DUP_TOP expr expr ROT_THREE STORE_SUBSCR
            if kv == 'kv':
                name = self.traverse(kv[-2], indent='');
                value = self.traverse(kv[1], indent=self.indent+(len(name)+2)*' ')
            else:
                name = self.traverse(kv[1], indent='');
                value = self.traverse(kv[-3], indent=self.indent+(len(name)+2)*' ')
            self.write(sep, name, ': ', value)
            sep = line_seperator
        self.write('}')
        self.indentLess(INDENT_PER_LEVEL)
        self.prune()
        

    def n_build_list(self, node):
        """
        prettyprint a list or tuple
        """
        lastnode = node.pop().type
        if lastnode.startswith('BUILD_LIST'):
            self.write('['); endchar = ']'
        elif lastnode.startswith('BUILD_TUPLE'):
            self.write('('); endchar = ')'
        elif lastnode.startswith('ROT_TWO'):
            self.write('('); endchar = ')'
        else:
            raise 'Internal Error: n_build_list expects list or tuple'
        
        self.indentMore(INDENT_PER_LEVEL)
        line_seperator = ',\n' + self.indent
        sep = INDENT_PER_LEVEL[:-1]
        for elem in node:
            if (elem == 'ROT_THREE'):
                continue

            assert elem == 'expr'
            value = self.traverse(elem)
            self.write(sep, value)
            sep = line_seperator
        self.write(endchar)
        self.indentLess(INDENT_PER_LEVEL)
        self.prune()


    def engine(self, entry, startnode):
        #self.print_("-----")
        #self.print_(str(startnode.__dict__))

        fmt = entry[0]
        ## no longer used, since BUILD_TUPLE_n is pretty printed:
        ##lastC = 0
        arg = 1
        i = 0

        m = escape.search(fmt)
        while m:
            i = m.end()
            self.write(m.group('prefix'))
                
            typ = m.group('type') or '{'
            node = startnode
            try:
                if m.group('child'):
                    node = node[int(m.group('child'))]
            except:
                print node.__dict__
                raise

            if   typ == '%':	self.write('%')
            elif typ == '+':	self.indentMore()
            elif typ == '-':	self.indentLess()
            elif typ == '|':	self.write(self.indent)
            ## no longer used, since BUILD_TUPLE_n is pretty printed:
            ##elif typ == ',':
            ##    if lastC == 1:
            ##        self.write(',')
            elif typ == 'c':
                self.preorder(node[entry[arg]])
                arg += 1
            elif typ == 'C':
                low, high, sep = entry[arg]
                ## lastC = remaining = len(node[low:high])
                remaining = len(node[low:high])
                for subnode in node[low:high]:
                    self.preorder(subnode)
                    remaining -= 1
                    if remaining > 0:
                        self.write(sep)
                arg += 1
            elif typ == '{':
                d = node.__dict__
                expr = m.group('expr')
                try:
                    self.f.write(eval(expr, d, d))
                except:
                    print node
                    raise
            m = escape.search(fmt, i)
        self.write(fmt[i:])
        
    def default(self, node):
           mapping = MAP.get(node, MAP_DIRECT)
           table = mapping[0]
           key = node

           for i in mapping[1:]:
              key = key[i]

           if table.has_key(key):
              self.engine(table[key], node)
              self.prune()

    def customize(self, customize):
       """
       Special handling for opcodes that take a variable number
       of arguments -- we add a new entry for each in TABLE_R.
       """
       for k, v in customize.items():
          if TABLE_R.has_key(k):
             continue
          op = k[ :k.rfind('_') ]
          if op == 'CALL_FUNCTION':	TABLE_R[k] = ('%c(%C)', 0, (1,-1,', '))
          elif op in ('CALL_FUNCTION_VAR',
                      'CALL_FUNCTION_VAR_KW', 'CALL_FUNCTION_KW'):
             if v == 0:
                str = '%c(%C' # '%C' is a dummy here ...
                p2 = (0, 0, None) # .. because of this
             else:
                str = '%c(%C, '
                p2 = (1,-2, ', ')
             if op == 'CALL_FUNCTION_VAR':
                str += '*%c)'
                entry = (str, 0, p2, -2)
             elif op == 'CALL_FUNCTION_KW':
                str += '**%c)'
                entry = (str, 0, p2, -2)
             else:
                str += '*%c, **%c)'
                if p2[2]: p2 = (1, -3, ', ')
                entry = (str, 0, p2, -3, -2)
             TABLE_R[k] = entry
          ## handled by n_mapexpr:
          ##if op == 'BUILD_SLICE':	TABLE_R[k] = ('%C'    ,    (0,-1,':'))
          ## handled by n_build_list:
          ##if   op == 'BUILD_LIST':	TABLE_R[k] = ('[%C]'  ,    (0,-1,', '))
          ##elif op == 'BUILD_TUPLE':	TABLE_R[k] = ('(%C%,)',    (0,-1,', '))

    def get_tuple_parameter(self, ast, name):
       """
       If the name of the formal parameter starts with dot,
       it's a tuple parameter, like this:
       #          def MyFunc(xx, (a,b,c), yy):
       #                  print a, b*2, c*42
       In byte-code, the whole tuple is assigned to parameter '.1' and
       then the tuple gets unpacked to 'a', 'b' and 'c'.

       Since identifiers starting with a dot are illegal in Python,
       we can search for the byte-code equivalent to '(a,b,c) = .1'
       """
       assert ast == 'stmts'
       for i in range(len(ast)):
           # search for an assign-statement
           assert ast[i] == 'stmt'
           node = ast[i][0]
           if node == 'assign' \
              and node[0] == ASSIGN_TUPLE_PARAM(name):
               # okay, this assigns '.n' to something
               del ast[i]
               # walk lhs; this
               # returns a tuple of identifiers as used
               # within the function definition
               assert node[1] == 'designator'
               # if lhs is not a UNPACK_TUPLE (or equiv.),
               # add parenteses to make this a tuple
               if node[1][0] not in ('unpack', 'unpack_list'):
                   return '(' + self.traverse(node[1], indent='') + ')'
               return self.traverse(node[1], indent='')
       raise "Can't find tuple parameter" % name


    def make_function(self, node, isLambda, nested=1):
        """Dump function defintion, doc string, and function body."""

        def build_param(ast, name, default):
            """build parameters:
                - handle defaults
                - handle format tuple parameters
            """
            # if formal parameter is a tuple, the paramater name
            # starts with a dot (eg. '.1', '.2')
            if name.startswith('.'):
                # replace the name with the tuple-string
                name = self.get_tuple_parameter(ast, name)

            if default:
                if self.showast:
                    print '--', name
                    print default
                    print '--'
                result = '%s = %s' % (name, self.traverse(default, indent='') )
                if result[-2:] == '= ':	# default was 'LOAD_CONST None'
                    result += 'None'
                return result
            else:
                return name
        defparams = node[:node[-1].attr] # node[-1] == MAKE_xxx_n
        code = node[-2].attr

        assert type(code) == CodeType
        code = Code(code, self.scanner)
        #assert isinstance(code, Code)

        ast = self.build_ast(code._tokens, code._customize)
        code._tokens = None # save memory
        assert ast == 'stmts'
        if isLambda:
            # convert 'return' statement to expression
            #assert len(ast[0]) == 1  wrong, see 'lambda (r,b): r,b,g'
            assert ast[-1] == 'stmt'
            assert len(ast[-1]) == 1
            assert ast[-1][0] == 'return_stmt'
            ast[-1][0].type = 'return_lambda'
        else:
            if ast[-1] == RETURN_NONE:
                # Python adds a 'return None' to the
                # end of any function; remove it
                ast.pop() # remove last node

        # add defaults values to parameter names
        argc = code.co_argcount
        paramnames = list(code.co_varnames[:argc])

        # defaults are for last n parameters, thus reverse
        paramnames.reverse(); defparams.reverse()

        # build parameters
       
        ##This would be a nicer piece of code, but I can't get this to work
        ## now, have to find a usable lambda constuct  hG/2000-09-05
        ##params = map(lambda name, default: build_param(ast, name, default),
        ##	     paramnames, defparams)
        params = []
        for name, default in map(lambda a,b: (a,b), paramnames, defparams):
            params.append( build_param(ast, name, default) )

        params.reverse() # back to correct order

        if 4 & code.co_flags:	# flag 2 -> variable number of args
            params.append('*%s' % code.co_varnames[argc])
            argc += 1
        if 8 & code.co_flags:	# flag 3 -> keyword args
            params.append('**%s' % code.co_varnames[argc])
            argc += 1

        # dump parameter list (with default values)
        indent = self.indent
        if isLambda:
            self.write("lambda ", ", ".join(params), ":")
        else:
            self.print_("(", ", ".join(params), "):")
            #self.print_(indent, '#flags:\t', int(code.co_flags))

        if code.co_consts and code.co_consts[0] != None: 
            # docstring exists, dump it
            self.print_docstring(indent, code.co_consts[0])

        for g in find_globals(ast, {}).keys():
           self.print_(indent, 'global ', g)
        self.gen_source(ast, code._customize, isLambda=isLambda)
        code._tokens = None; code._customize = None # save memory


    def build_class(self, code):
        """Dump class definition, doc string and class body."""

        assert type(code) == CodeType
        code = Code(code, self.scanner)
        #assert isinstance(code, Code)

        indent = self.indent
        #self.print_(indent, '#flags:\t', int(code.co_flags))
        ast = self.build_ast(code._tokens, code._customize)
        code._tokens = None # save memory
        assert ast == 'stmts'

        # if docstring exists, dump it
        if code.co_consts[0] != None \
           and ast[0] == ASSIGN_DOC_STRING(code.co_consts[0]):
            #print '\n\n>>-->>doc string set\n\n'
            self.print_docstring(indent, code.co_consts[0])
            del ast[0]

        # the function defining a class normally returns locals(); we
        # don't want this to show up in the source, thus remove the node
        if ast[-1] == RETURN_LOCALS:
            ast.pop() # remove last node

        for g in find_globals(ast, {}).keys():
           self.print_(indent, 'global ', g)
        self.gen_source(ast, code._customize)
        code._tokens = None; code._customize = None # save memory


    def gen_source(self, ast, customize, isLambda=0):
        """convert AST to source code"""

        # if code would be empty, append 'pass'
        if len(ast) == 0:
            self.print_(self.indent, 'pass')
        else:
            self.customize(customize)
            self.print_(self.traverse(ast, isLambda=isLambda))


    def build_ast(self, tokens, customize):
        assert type(tokens) == ListType
        assert isinstance(tokens[0], Token)

        # Build AST from disassembly.
        try:
            ast = Parser.parse(tokens, customize)
        except Parser.ParserError, e:
            raise ParserError(e, tokens)

        if self.showast:
            self.print_(repr(ast))
        return ast
