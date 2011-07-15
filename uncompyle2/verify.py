#
# (C) Copyright 2000-2002 by hartmut Goebel <hartmut@goebel.noris.de>
#
# byte-code verifier for uncompyle
#

import types
import uncompyle, Scanner

JUMP_OPs = None

#--- exceptions ---

class VerifyCmpError(Exception):
	pass

class CmpErrorConsts(VerifyCmpError):
	"""Exception to be raised when consts differ."""
	def __init__(self, name, index):
		self.name = name
		self.index = index

	def __str__(self):
		return 'Compare Error within Consts of %s at index %i' % \
		       (repr(self.name), self.index)
					
class CmpErrorConstsType(VerifyCmpError):
	"""Exception to be raised when consts differ."""
	def __init__(self, name, index):
		self.name = name
		self.index = index

	def __str__(self):
		return 'Consts type differ in %s at index %i' % \
		       (repr(self.name), self.index)

class CmpErrorConstsLen(VerifyCmpError):
	"""Exception to be raised when length of co_consts differs."""
	def __init__(self, name, consts1, consts2):
		self.name = name
		self.consts = (consts1, consts2)

	def __str__(self):
		return 'Consts length differs in %s:\n\n%i:\t%s\n\n%i:\t%s\n\n' % \
		       (repr(self.name),
			len(self.consts[0]), `self.consts[0]`,
			len(self.consts[1]), `self.consts[1]`)
					
class CmpErrorCode(VerifyCmpError):
	"""Exception to be raised when code differs."""
	def __init__(self, name, index, token1, token2, tokens1, tokens2):
		self.name = name
		self.index = index
		self.token1 = token1
		self.token2 = token2
		self.tokens = [tokens1, tokens2]
		
	def __str__(self):
		s =  reduce(lambda s,t: "%s%-37s\t%-37s\n" % (s, t[0], t[1]),
			      map(lambda a,b: (a,b),
				  self.tokens[0],
				  self.tokens[1]),
			      'Code differs in %s\n' % str(self.name))
		return ('Code differs in %s at offset %i [%s] != [%s]\n\n' % \
		       (repr(self.name), self.index,
			repr(self.token1), repr(self.token2))) + s

class CmpErrorCodeLen(VerifyCmpError):
	"""Exception to be raised when code length differs."""
	def __init__(self, name, tokens1, tokens2):
		self.name = name
		self.tokens = [tokens1, tokens2]

	def __str__(self):
		return reduce(lambda s,t: "%s%-37s\t%-37s\n" % (s, t[0], t[1]),
			      map(lambda a,b: (a,b),
				  self.tokens[0],
				  self.tokens[1]),
			      'Code len differs in %s\n' % str(self.name))

class CmpErrorMember(VerifyCmpError):
	"""Exception to be raised when other members differ."""
	def __init__(self, name, member, data1, data2):
		self.name = name
		self.member = member
		self.data = (data1, data2)

	def __str__(self):
		return 'Member %s differs in %s:\n\t%s\n\t%s\n' % \
		       (repr(self.member), repr(self.name),
			repr(self.data[0]), repr(self.data[1]))

#--- compare ---
					
# these members are ignored
__IGNORE_CODE_MEMBERS__ = ['co_filename', 'co_firstlineno', 'co_lnotab', 'co_consts']

def cmp_code_objects(version, code_obj1, code_obj2, name=''):
	"""
	Compare two code-objects.

	This is the main part of this module.
	"""
	#print code_obj1, type(code_obj2)
	assert type(code_obj1) == types.CodeType
	assert type(code_obj2) == types.CodeType
	#print dir(code_obj1)
	if isinstance(code_obj1, object):
		# new style classes (Python 2.2)
		# assume _both_ code objects to be new stle classes
		assert dir(code_obj1) == dir(code_obj2)
	else:
		# old style classes
		assert dir(code_obj1) == code_obj1.__members__
		assert dir(code_obj2) == code_obj2.__members__
		assert code_obj1.__members__ == code_obj2.__members__
	
	if name == '__main__':
		name = code_obj1.co_name
	else:
		name = '%s.%s' % (name, code_obj1.co_name)
		if name == '.?': name = '__main__'
		
	if isinstance(code_obj1, object) and cmp(code_obj1, code_obj2):
		# use the new style code-classes' __cmp__ method, which
		# should be faster and more sophisticated
		# if this compare fails, we use the old routine to
		# find out, what exactly is nor equal
		# if this compare succeds, simply return
		#return
		pass

	if isinstance(code_obj1, object):
		members = filter(lambda x: x.startswith('co_'), dir(code_obj1))
	else:
		members = dir(code_obj1);
	members.sort(); #members.reverse()

	tokens1 = None
	for member in members:
		if member in __IGNORE_CODE_MEMBERS__:
			pass
		elif member == 'co_code':
			scanner = Scanner.getscanner(version)
			scanner.setShowAsm( showasm=0 )
			global JUMP_OPs
			JUMP_OPs = scanner.JUMP_OPs
			
			# use changed Token class
			#   we (re)set this here to save exception handling,
			#   which would get 'unubersichtlich'
			scanner.setTokenClass(Token)
			try:
				# disassemble both code-objects
				tokens1,customize = scanner.disassemble(code_obj1)
				del customize # save memory
				tokens2,customize = scanner.disassemble(code_obj2)
				del customize # save memory
			finally:
				scanner.resetTokenClass() # restore Token class

			# compare length
			if len(tokens1) != len(tokens2):
				raise CmpErrorCodeLen(name, tokens1, tokens2)
			# compare contents
			#print len(tokens1), type(tokens1), type(tokens2)
			for i in xrange(len(tokens1)):
				if tokens1[i] != tokens2[i]:
					#print '-->', i, type(tokens1[i]), type(tokens2[i])
					raise CmpErrorCode(name, i, tokens1[i],
							   tokens2[i], tokens1, tokens2)
			del tokens1, tokens2 # save memory
		elif member == 'co_consts':
			# compare length
			if len(code_obj1.co_consts) != len(code_obj2.co_consts):
				raise CmpErrorConstsLen(name, code_obj1.co_consts ,code_obj2.co_consts)
			# compare contents
			for idx in xrange(len(code_obj1.co_consts)):
				const1 = code_obj1.co_consts[idx]
				const2 = code_obj2.co_consts[idx]
				## print code_obj1.co_consts[idx], '\t',
				## print code_obj2.co_consts[idx]
				# same type?
				if type(const1) != type(const2):
					raise CmpErrorConstsType(name, idx)
				if type(const1) == types.CodeType:
					# code object -> recursive compare
					cmp_code_objects(version, const1,
							 const2, name=name)
				elif cmp(const1, const2) != 0:
					# content differs
					raise CmpErrorConsts(name, idx)
		else:
			# all other members must be equal
			if getattr(code_obj1, member) != getattr(code_obj2, member):
				raise CmpErrorMember(name, member,
						     getattr(code_obj1,member),
						     getattr(code_obj2,member))

class Token(Scanner.Token):
	"""Token class with changed semantics for 'cmp()'."""

	def __cmp__(self, o):
		t = self.type # shortcut
		if t in JUMP_OPs:
			# ignore offset
			return cmp(t, o.type)
		else:
			return cmp(t, o.type) \
			       or cmp(self.pattr, o.pattr)

	def __repr__(self):
		return '%s %s (%s)' % (str(self.type), str(self.attr),
				       repr(self.pattr))


def compare_code_with_srcfile(pyc_filename, src_filename):
	"""Compare a .pyc with a source code file."""
	version, code_obj1 = uncompyle._load_module(pyc_filename)
	code_obj2 = uncompyle._load_file(src_filename)
	cmp_code_objects(version, code_obj1, code_obj2)

def compare_files(pyc_filename1, pyc_filename2):
	"""Compare two .pyc files."""
	version, code_obj1 = uncompyle._load_module(pyc_filename1)
	version, code_obj2 = uncompyle._load_module(pyc_filename2)
	cmp_code_objects(version, code_obj1, code_obj2)

if __name__ == '__main__':
	t1 = Token('LOAD_CONST', None, 'code_object _expandLang', 52)
	t2 = Token('LOAD_CONST', -421, 'code_object _expandLang', 55)
	print `t1`
	print `t2`
	print cmp(t1, t2), cmp(t1.type, t2.type), cmp(t1.attr, t2.attr)
