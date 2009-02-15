#  Copyright (c) 1999 John Aycock
#  Copyright (c) 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#  Copyright (c) 2005 by Dan Pascu <dan@windowmaker.org>
#
#  See main module for license.
#

__all__ = ['parse', 'AST', 'ParserError', 'Parser']

from spark import GenericASTBuilder, GenericASTMatcher
import string, exceptions, sys
from UserList import UserList

from Scanner import Token

class AST(UserList):
	def __init__(self, type, kids=[]):
		self.type = intern(type)
		UserList.__init__(self, kids)

	def __getslice__(self, low, high):	return self.data[low:high]
	def __eq__(self, o):
		if isinstance(o, AST):
			return self.type == o.type \
			       and UserList.__eq__(self, o)
		else:
			return self.type == o

	def __hash__(self):			return hash(self.type)

	def __repr__(self, indent=''):
		rv = str(self.type)
		for k in self:
			rv = rv + '\n' + string.replace(str(k), '\n', '\n   ')
		return rv


class ParserError(Exception):
    def __init__(self, token, offset):
        self.token = token
        self.offset = offset

    def __str__(self):
        return "Syntax error at or near `%r' token at offset %s" % \
               (self.token, self.offset)
    

class Parser(GenericASTBuilder):
	def __init__(self):
		GenericASTBuilder.__init__(self, AST, 'stmts')
		self.customized = {}

	def cleanup(self):
		"""
		Remove recursive references to allow garbage
		collector to collect this object.
		"""
		for dict in (self.rule2func, self.rules, self.rule2name, self.first):
			for i in dict.keys():
				dict[i] = None
		for i in dir(self):
			setattr(self, i, None)

	def error(self, token):
            raise ParserError(token, token.offset)

	def typestring(self, token):
		return token.type
	
	def p_funcdef(self, args):
		'''
		stmt ::= funcdef
		funcdef ::= mkfunc designator
		load_closure ::= load_closure LOAD_CLOSURE
		load_closure ::= LOAD_CLOSURE
		'''

	def p_list_comprehension(self, args):
		'''
		expr ::= list_compr
		list_compr ::= BUILD_LIST_0 DUP_TOP LOAD_ATTR
				designator list_iter del_stmt

		list_iter ::= list_for
		list_iter ::= list_if
		list_iter ::= lc_body

		list_for ::= expr _for designator list_iter
				JUMP_ABSOLUTE COME_FROM
		list_if ::= expr condjmp list_iter
				_jump COME_FROM POP_TOP
				COME_FROM

		lc_body ::= LOAD_NAME expr CALL_FUNCTION_1 POP_TOP
		lc_body ::= LOAD_FAST expr CALL_FUNCTION_1 POP_TOP
		'''
		
	def p_augmented_assign(self, args):
		'''
		stmt ::= augassign1
		stmt ::= augassign2
		augassign1 ::= expr expr inplace_op designator
		augassign1 ::= expr expr inplace_op ROT_THREE STORE_SUBSCR
		augassign1 ::= expr expr inplace_op ROT_TWO   STORE_SLICE+0
		augassign1 ::= expr expr inplace_op ROT_THREE STORE_SLICE+1
		augassign1 ::= expr expr inplace_op ROT_THREE STORE_SLICE+2
		augassign1 ::= expr expr inplace_op ROT_FOUR  STORE_SLICE+3
		augassign2 ::= expr DUP_TOP LOAD_ATTR expr
				inplace_op ROT_TWO   STORE_ATTR

		inplace_op ::= INPLACE_ADD
		inplace_op ::= INPLACE_SUBTRACT
		inplace_op ::= INPLACE_MULTIPLY
		inplace_op ::= INPLACE_DIVIDE
		inplace_op ::= INPLACE_TRUE_DIVIDE
		inplace_op ::= INPLACE_FLOOR_DIVIDE
		inplace_op ::= INPLACE_MODULO
		inplace_op ::= INPLACE_POWER
		inplace_op ::= INPLACE_LSHIFT
		inplace_op ::= INPLACE_RSHIFT
		inplace_op ::= INPLACE_AND
		inplace_op ::= INPLACE_XOR
		inplace_op ::= INPLACE_OR 
		'''

	def p_assign(self, args):
		'''
		stmt ::= assign
		assign ::= expr DUP_TOP designList
		assign ::= expr designator
		'''

	def p_print(self, args):
		'''
		stmt ::= print_stmt
		stmt ::= print_stmt_nl
		stmt ::= print_nl_stmt
		print_stmt ::= expr PRINT_ITEM
		print_nl_stmt ::= PRINT_NEWLINE
		print_stmt_nl ::= print_stmt print_nl_stmt
		'''

	def p_print_to(self, args):
		'''
		stmt ::= print_to
		stmt ::= print_to_nl
		stmt ::= print_nl_to
		print_to ::= expr print_to_items POP_TOP
		print_to_nl ::= expr print_to_items PRINT_NEWLINE_TO
		print_nl_to ::= expr PRINT_NEWLINE_TO
		print_to_items ::= print_to_items print_to_item
		print_to_items ::= print_to_item
		print_to_item ::= DUP_TOP expr ROT_TWO PRINT_ITEM_TO
		'''
		# expr   print_to*   POP_TOP
		# expr { print_to* } PRINT_NEWLINE_TO

	def p_import15(self, args):
		'''
		stmt ::= importstmt
		stmt ::= importfrom

		importstmt ::= IMPORT_NAME STORE_FAST
		importstmt ::= IMPORT_NAME STORE_NAME

		importfrom ::= IMPORT_NAME importlist POP_TOP
		importlist ::= importlist IMPORT_FROM
		importlist ::= IMPORT_FROM
		'''

	def p_import20(self, args):
		'''
		stmt ::= importstmt2
		stmt ::= importfrom2
		stmt ::= importstar2

		importstmt2 ::= LOAD_CONST import_as
		importstar2 ::= LOAD_CONST IMPORT_NAME IMPORT_STAR

		importfrom2 ::= LOAD_CONST IMPORT_NAME importlist2 POP_TOP
		importlist2 ::= importlist2 import_as
		importlist2 ::= import_as
		import_as ::= IMPORT_NAME designator
		import_as ::= IMPORT_NAME LOAD_ATTR designator
		import_as ::= IMPORT_FROM designator

        stmt ::= importstmt25
        stmt ::= importfrom25
        stmt ::= importstar25

        importstmt25 ::= LOAD_CONST LOAD_CONST import_as importstmt25:: = LOAD_CONST LOAD_CONST import_as
        importstar25 ::= LOAD_CONST LOAD_CONST IMPORT_NAME IMPORT_STAR importstar25:: = LOAD_CONST LOAD_CONST IMPORT_NAME IMPORT_STAR
        importfrom25 ::= LOAD_CONST LOAD_CONST IMPORT_NAME importlist2 POP_TOP importfrom25:: = LOAD_CONST LOAD_CONST IMPORT_NAME importlist2 POP_TOP
		'''

	def p_grammar(self, args):
		'''
		stmts ::= stmts stmt
		stmts ::= stmt

		stmts_opt ::= stmts
		stmts_opt ::= passstmt
		passstmt ::= 

		designList ::= designator designator
		designList ::= designator DUP_TOP designList

		designator ::= STORE_FAST
		designator ::= STORE_NAME
		designator ::= STORE_GLOBAL
		designator ::= STORE_DEREF
		designator ::= expr STORE_ATTR
		designator ::= expr STORE_SLICE+0
		designator ::= expr expr STORE_SLICE+1
		designator ::= expr expr STORE_SLICE+2
		designator ::= expr expr expr STORE_SLICE+3
		designator ::= store_subscr
		store_subscr ::= expr expr STORE_SUBSCR
		designator ::= unpack
		designator ::= unpack_list
		
		stmt ::= classdef
		stmt ::= call_stmt
		call_stmt ::= expr POP_TOP

		stmt ::= return_stmt
		return_stmt ::= expr RETURN_VALUE

		stmt ::= yield_stmt
		yield_stmt ::= expr YIELD_STMT
		yield_stmt ::= expr YIELD_VALUE

		stmt ::= break_stmt
		break_stmt ::= BREAK_LOOP
		
		stmt ::= continue_stmt
		continue_stmt ::= JUMP_ABSOLUTE
		continue_stmt ::= CONTINUE_LOOP
		
		stmt ::= raise_stmt
		raise_stmt ::= exprlist RAISE_VARARGS
		raise_stmt ::= nullexprlist RAISE_VARARGS
		
		stmt ::= exec_stmt
		exec_stmt ::= expr exprlist DUP_TOP EXEC_STMT
		exec_stmt ::= expr exprlist EXEC_STMT
		
		stmt ::= assert
		stmt ::= assert2
		stmt ::= assert3
		stmt ::= assert4
		stmt ::= ifstmt
		stmt ::= ifelsestmt
		stmt ::= whilestmt
		stmt ::= while1stmt
		stmt ::= whileelsestmt
		stmt ::= while1elsestmt
		stmt ::= forstmt
		stmt ::= forelsestmt
		stmt ::= trystmt
		stmt ::= tryfinallystmt
		
		stmt ::= del_stmt
		del_stmt ::= DELETE_FAST
		del_stmt ::= DELETE_NAME
		del_stmt ::= DELETE_GLOBAL
		del_stmt ::= expr DELETE_SLICE+0
		del_stmt ::= expr expr DELETE_SLICE+1
		del_stmt ::= expr expr DELETE_SLICE+2
		del_stmt ::= expr expr expr DELETE_SLICE+3
		del_stmt ::= delete_subscr
		delete_subscr ::= expr expr DELETE_SUBSCR
		del_stmt ::= expr DELETE_ATTR
		
		kwarg   ::= LOAD_CONST expr
		
		classdef ::= LOAD_CONST expr mkfunc
			        CALL_FUNCTION_0 BUILD_CLASS designator

		condjmp    ::= JUMP_IF_FALSE POP_TOP
		condjmp    ::= JUMP_IF_TRUE  POP_TOP

		assert ::= expr JUMP_IF_FALSE POP_TOP
				expr JUMP_IF_TRUE POP_TOP
				LOAD_GLOBAL RAISE_VARARGS
				COME_FROM COME_FROM POP_TOP
		assert2 ::= expr JUMP_IF_FALSE POP_TOP
				expr JUMP_IF_TRUE POP_TOP
				LOAD_GLOBAL expr RAISE_VARARGS
				COME_FROM COME_FROM POP_TOP
		assert3 ::= expr JUMP_IF_TRUE POP_TOP
				LOAD_GLOBAL RAISE_VARARGS
				COME_FROM POP_TOP
		assert4 ::= expr JUMP_IF_TRUE POP_TOP
				LOAD_GLOBAL expr RAISE_VARARGS
				COME_FROM POP_TOP

		_jump ::= JUMP_ABSOLUTE
		_jump ::= JUMP_FORWARD

		ifstmt ::= expr condjmp stmts_opt
				_jump COME_FROM POP_TOP COME_FROM

		ifelsestmt ::= expr condjmp stmts_opt
				_jump COME_FROM
				POP_TOP stmts COME_FROM

		trystmt ::= SETUP_EXCEPT stmts_opt
				POP_BLOCK _jump
				COME_FROM except_stmt

		try_end  ::= END_FINALLY COME_FROM
		try_end  ::= except_else
		except_else ::= END_FINALLY COME_FROM stmts

		except_stmt ::= except_cond except_stmt COME_FROM
		except_stmt ::= except_conds try_end COME_FROM
		except_stmt ::= except try_end COME_FROM
		except_stmt ::= try_end

		except_conds ::= except_cond except_conds COME_FROM
		except_conds ::= 

		except_cond ::= except_cond1
		except_cond ::= except_cond2
		except_cond1 ::= DUP_TOP expr COMPARE_OP
				JUMP_IF_FALSE
				POP_TOP POP_TOP POP_TOP POP_TOP
				stmts_opt _jump COME_FROM
				POP_TOP
		except_cond2 ::= DUP_TOP expr COMPARE_OP
				JUMP_IF_FALSE
				POP_TOP POP_TOP designator POP_TOP
				stmts_opt _jump COME_FROM
				POP_TOP
		except  ::=  POP_TOP POP_TOP POP_TOP
				stmts_opt _jump

		tryfinallystmt ::= SETUP_FINALLY stmts_opt
				POP_BLOCK LOAD_CONST
				COME_FROM stmts_opt END_FINALLY

		whilestmt ::= SETUP_LOOP
				expr JUMP_IF_FALSE POP_TOP
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_TOP POP_BLOCK COME_FROM
		while1stmt ::= SETUP_LOOP
				JUMP_FORWARD JUMP_IF_FALSE POP_TOP COME_FROM
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_TOP POP_BLOCK COME_FROM
		whileelsestmt ::= SETUP_LOOP
		              	expr JUMP_IF_FALSE POP_TOP
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_TOP POP_BLOCK
				stmts COME_FROM
		while1elsestmt ::= SETUP_LOOP
		              	JUMP_FORWARD JUMP_IF_FALSE POP_TOP COME_FROM
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_TOP POP_BLOCK
				stmts COME_FROM

		_for ::= GET_ITER FOR_ITER
		_for ::= LOAD_CONST FOR_LOOP

		forstmt ::= SETUP_LOOP expr _for designator
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_BLOCK COME_FROM
		forelsestmt ::= SETUP_LOOP expr _for designator
				stmts_opt JUMP_ABSOLUTE
				COME_FROM POP_BLOCK stmts COME_FROM

		'''

	def p_expr(self, args):
		'''
		expr ::= load_closure mklambda
		expr ::= mklambda
		expr ::= SET_LINENO
		expr ::= LOAD_FAST
		expr ::= LOAD_NAME
		expr ::= LOAD_CONST
		expr ::= LOAD_GLOBAL
		expr ::= LOAD_DEREF
		expr ::= LOAD_LOCALS
		expr ::= expr LOAD_ATTR
		expr ::= binary_expr
		expr ::= build_list
		
		binary_expr ::= expr expr binary_op
		binary_op ::= BINARY_ADD
		binary_op ::= BINARY_SUBTRACT
		binary_op ::= BINARY_MULTIPLY
		binary_op ::= BINARY_DIVIDE
		binary_op ::= BINARY_TRUE_DIVIDE
		binary_op ::= BINARY_FLOOR_DIVIDE
		binary_op ::= BINARY_MODULO
		binary_op ::= BINARY_LSHIFT
		binary_op ::= BINARY_RSHIFT
		binary_op ::= BINARY_AND
		binary_op ::= BINARY_OR
		binary_op ::= BINARY_XOR
		binary_op ::= BINARY_POWER

		expr ::= binary_subscr
		binary_subscr ::= expr expr BINARY_SUBSCR
		expr ::= expr expr DUP_TOPX_2 BINARY_SUBSCR
		expr ::= cmp
		expr ::= expr UNARY_POSITIVE
		expr ::= expr UNARY_NEGATIVE
		expr ::= expr UNARY_CONVERT
		expr ::= expr UNARY_INVERT
		expr ::= expr UNARY_NOT
		expr ::= mapexpr
		expr ::= expr SLICE+0
		expr ::= expr expr SLICE+1
		expr ::= expr expr SLICE+2
		expr ::= expr expr expr SLICE+3
		expr ::= expr DUP_TOP SLICE+0
		expr ::= expr expr DUP_TOPX_2 SLICE+1
		expr ::= expr expr DUP_TOPX_2 SLICE+2
		expr ::= expr expr expr DUP_TOPX_3 SLICE+3
		expr ::= and
		expr ::= and2
		expr ::= or
		or   ::= expr JUMP_IF_TRUE  POP_TOP expr COME_FROM
		and  ::= expr JUMP_IF_FALSE POP_TOP expr COME_FROM
		and2 ::= _jump JUMP_IF_FALSE POP_TOP COME_FROM expr COME_FROM

		cmp ::= cmp_list
		cmp ::= compare
		compare ::= expr expr COMPARE_OP
		cmp_list ::= expr cmp_list1 ROT_TWO POP_TOP
				COME_FROM
		cmp_list1 ::= expr DUP_TOP ROT_THREE
				COMPARE_OP JUMP_IF_FALSE POP_TOP
				cmp_list1 COME_FROM
		cmp_list1 ::= expr DUP_TOP ROT_THREE
				COMPARE_OP JUMP_IF_FALSE POP_TOP
				cmp_list2 COME_FROM
		cmp_list2 ::= expr COMPARE_OP JUMP_FORWARD
		mapexpr ::= BUILD_MAP kvlist

		kvlist ::= kvlist kv
		kvlist ::= kvlist kv2
		kvlist ::=

		kv ::= DUP_TOP expr ROT_TWO expr STORE_SUBSCR
		kv2 ::= DUP_TOP expr expr ROT_THREE STORE_SUBSCR

		exprlist ::= exprlist expr
		exprlist ::= expr

		nullexprlist ::=
		'''

	def nonterminal(self, nt, args):
		collect = ('stmts', 'exprlist', 'kvlist')

		if nt in collect and len(args) > 1:
			#
			#  Collect iterated thingies together.
			#
			rv = args[0]
			rv.append(args[1])
		else:
			rv = GenericASTBuilder.nonterminal(self, nt, args)
		return rv

	def __ambiguity(self, children):
		# only for debugging! to be removed hG/2000-10-15
		print children
		return GenericASTBuilder.ambiguity(self, children)

	def resolve(self, list):
		if len(list) == 2 and 'funcdef' in list and 'assign' in list:
			return 'funcdef'
		#print >> sys.stderr, 'resolve', str(list)
		return GenericASTBuilder.resolve(self, list)

nop = lambda self, args: None

p = Parser()

def parse(tokens, customize):
	#
	#  Special handling for opcodes that take a variable number
	#  of arguments -- we add a new rule for each:
	#
	#	expr ::= {expr}^n BUILD_LIST_n
	#	expr ::= {expr}^n BUILD_TUPLE_n
	#	expr ::= {expr}^n BUILD_SLICE_n
	#	unpack_list ::= UNPACK_LIST {expr}^n
	#	unpack ::= UNPACK_TUPLE {expr}^n
	#	unpack ::= UNPACK_SEQEUENE {expr}^n
	#	mkfunc ::= {expr}^n LOAD_CONST MAKE_FUNCTION_n
	#	mkfunc ::= {expr}^n load_closure LOAD_CONST MAKE_FUNCTION_n
	#	expr ::= expr {expr}^n CALL_FUNCTION_n
	#	expr ::= expr {expr}^n CALL_FUNCTION_VAR_n POP_TOP
	#	expr ::= expr {expr}^n CALL_FUNCTION_VAR_KW_n POP_TOP
	#	expr ::= expr {expr}^n CALL_FUNCTION_KW_n POP_TOP
	#
	global p
	for k, v in customize.items():
		# avoid adding the same rule twice to this parser
		if p.customized.has_key(k):
			continue
		p.customized[k] = None

		#nop = lambda self, args: None
		op = k[:string.rfind(k, '_')]
		if op in ('BUILD_LIST', 'BUILD_TUPLE'):
			rule = 'build_list ::= ' + 'expr '*v + k
		elif op == 'BUILD_SLICE':
			rule = 'expr ::= ' + 'expr '*v + k
		elif op in ('UNPACK_TUPLE', 'UNPACK_SEQUENCE'):
			rule = 'unpack ::= ' + k + ' designator'*v
		elif op == 'UNPACK_LIST':
			rule = 'unpack_list ::= ' + k + ' designator'*v
		elif op == 'DUP_TOPX':
			# no need to add a rule
			continue
			#rule = 'dup_topx ::= ' + 'expr '*v + k
		elif op == 'MAKE_FUNCTION':
			p.addRule('mklambda ::= %s LOAD_LAMBDA %s' %
				  ('expr '*v, k), nop)
			rule = 'mkfunc ::= %s LOAD_CONST %s' % ('expr '*v, k)
		elif op == 'MAKE_CLOSURE':
			p.addRule('mklambda ::= %s load_closure LOAD_LAMBDA %s' %
				  ('expr '*v, k), nop)
			rule = 'mkfunc ::= %s load_closure LOAD_CONST %s' % ('expr '*v, k)
		elif op in ('CALL_FUNCTION', 'CALL_FUNCTION_VAR',
			    'CALL_FUNCTION_VAR_KW', 'CALL_FUNCTION_KW'):
			na = (v & 0xff)           # positional parameters
			nk = (v >> 8) & 0xff      # keyword parameters
			# number of apply equiv arguments:
			nak = ( len(op)-len('CALL_FUNCTION') ) / 3
			rule = 'expr ::= expr ' + 'expr '*na + 'kwarg '*nk \
			       + 'expr ' * nak + k
		else:
			raise 'unknown customize token %s' % k
		p.addRule(rule, nop)
	ast = p.parse(tokens)
	#p.cleanup()
	return ast

# local variables:
# tab-width: 4
