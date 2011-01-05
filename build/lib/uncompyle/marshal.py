"""Internal Python object serialization

This module contains functions that can read and write Python values in a binary format. The format is specific to Python, but independent of machine architecture issues (e.g., you can write a Python value to a file on a PC, transport the file to a Sun, and read it back there). Details of the format may change between Python versions.
"""

import types
from _codecs import utf_8_decode, utf_8_encode

TYPE_NULL     = '0'
TYPE_NONE     = 'N'
TYPE_FALSE    = 'F'
TYPE_TRUE     = 'T'
TYPE_STOPITER = 'S'
TYPE_ELLIPSIS = '.'
TYPE_INT      = 'i'
TYPE_INT64    = 'I'
TYPE_FLOAT    = 'f'
TYPE_BINARY_FLOAT = 'g'
TYPE_COMPLEX  = 'x'
TYPE_LONG     = 'l'
TYPE_STRING   = 's'
TYPE_INTERNED = 't'
TYPE_STRINGREF= 'R'
TYPE_TUPLE    = '('
TYPE_LIST     = '['
TYPE_DICT     = '{'
TYPE_CODE     = 'c'
TYPE_UNICODE  = 'u'
TYPE_UNKNOWN  = '?'
TYPE_SET      = '<'
TYPE_FROZENSET= '>'

class _NULL:
    pass

class _Unmarshaller:

    dispatch = {}

    def __init__(self, readfunc):
        self._read = readfunc
        self._stringtable = []

    def load(self):
        c = self._read(1)
        if not c:
            raise EOFError
        try:
            return self.dispatch[c](self)
        except KeyError:
            raise ValueError, "bad marshal code: %c (%d)" % (c, ord(c))

    def r_short(self):
        lo = ord(self._read(1))
        hi = ord(self._read(1))
        x = lo | (hi<<8)
        if x & 0x8000:
            x = x - 0x10000
        return x

    def r_long(self):
        s = self._read(4)
        a = ord(s[0])
        b = ord(s[1])
        c = ord(s[2])
        d = ord(s[3])
        x = a | (b<<8) | (c<<16) | (d<<24)
        if d & 0x80 and x > 0:
            x = -((1L<<32) - x)
            return int(x)
        else:
            return x

    def r_long64(self):
        a = ord(self._read(1))
        b = ord(self._read(1))
        c = ord(self._read(1))
        d = ord(self._read(1))
        e = long(ord(self._read(1)))
        f = long(ord(self._read(1)))
        g = long(ord(self._read(1)))
        h = long(ord(self._read(1)))
        x = a | (b<<8) | (c<<16) | (d<<24)
        x = x | (e<<32) | (f<<40) | (g<<48) | (h<<56)
        if h & 0x80 and x > 0:
            x = -((1L<<64) - x)
        return x

    def load_null(self):
        return _NULL
    dispatch[TYPE_NULL] = load_null

    def load_none(self):
        return None
    dispatch[TYPE_NONE] = load_none

    def load_true(self):
        return True
    dispatch[TYPE_TRUE] = load_true

    def load_false(self):
        return False
    dispatch[TYPE_FALSE] = load_false

    def load_stopiter(self):
        return StopIteration
    dispatch[TYPE_STOPITER] = load_stopiter

    def load_ellipsis(self):
        return Ellipsis
    dispatch[TYPE_ELLIPSIS] = load_ellipsis

    dispatch[TYPE_INT] = r_long

    dispatch[TYPE_INT64] = r_long64

    def load_long(self):
        size = self.r_long()
        sign = 1
        if size < 0:
            sign = -1
            size = -size
        x = 0L
        for i in range(size):
            d = self.r_short()
            x = x | (d<<(i*15L))
        return x * sign
    dispatch[TYPE_LONG] = load_long

    def load_float(self):
        n = ord(self._read(1))
        s = self._read(n)
        return float(s)
    dispatch[TYPE_FLOAT] = load_float

    def load_binary_float(self):
        # From: http://billyearney-skulpt.googlecode.com/hg/src/skc1.py

        # bleh, straight port of PyFloat_Unpack8. not sure if it's right at
        # all, but works for simple stuff
        data = self._read(8)
        off = 7

        # First byte
        sign = (ord(data[off]) >> 7) & 1
        e = (ord(data[off]) & 0x7f) << 4
        off -= 1

        # Second byte
        e |= ord(data[off]) >> 4 & 0xf
        fhi = (ord(data[off]) & 0xf) << 24
        off -= 1

        if e == 2047:
            raise ValueError("can't unpack IEEE 754 special value")

        # Third byte
        fhi |= ord(data[off]) << 16
        off -= 1

        # Fourth byte
        fhi |= ord(data[off]) << 8
        off -= 1

        # Fifth byte
        fhi |= ord(data[off])
        off -= 1

        # Sixth byte
        flo = ord(data[off]) << 16
        off -= 1

        # Seventh byte
        flo |= ord(data[off]) << 8
        off -= 1

        # Eighth byte
        flo |= ord(data[off])

        x = float(fhi) + float(flo) / 16777216.0 # 2**24
        x /= 268435456.0 # 2**28

        if e == 0:
            e = -1022
        else:
            x += 1.0
            e -= 1023

        # x = ldexp(x, e)
        x = x * (2**e)

        if sign:
            x = -x

        return x
    dispatch[TYPE_BINARY_FLOAT] = load_binary_float

    def load_complex(self):
        n = ord(self._read(1))
        s = self._read(n)
        real = float(s)
        n = ord(self._read(1))
        s = self._read(n)
        imag = float(s)
        return complex(real, imag)
    dispatch[TYPE_COMPLEX] = load_complex

    def load_string(self):
        n = self.r_long()
        return self._read(n)
    dispatch[TYPE_STRING] = load_string

    def load_interned(self):
        n = self.r_long()
        ret = intern(self._read(n))
        self._stringtable.append(ret)
        return ret
    dispatch[TYPE_INTERNED] = load_interned

    def load_stringref(self):
        n = self.r_long()
        return self._stringtable[n]
    dispatch[TYPE_STRINGREF] = load_stringref

    def load_unicode(self):
        n = self.r_long()
        s = self._read(n)
        #ret = s.decode('utf8')
        ret, len_ret = utf_8_decode(s)
        return ret
    dispatch[TYPE_UNICODE] = load_unicode

    def load_tuple(self):
        return tuple(self.load_list())
    dispatch[TYPE_TUPLE] = load_tuple

    def load_list(self):
        n = self.r_long()
        list = [self.load() for i in range(n)]
        return list
    dispatch[TYPE_LIST] = load_list

    def load_dict(self):
        d = {}
        while 1:
            key = self.load()
            if key is _NULL:
                break
            value = self.load()
            d[key] = value
        return d
    dispatch[TYPE_DICT] = load_dict

    def load_code(self):
        argcount = self.r_long()
        nlocals = self.r_long()
        stacksize = self.r_long()
        flags = self.r_long()
        code = self.load()
        consts = self.load()
        names = self.load()
        varnames = self.load()
        freevars = self.load()
        cellvars = self.load()
        filename = self.load()
        name = self.load()
        firstlineno = self.r_long()
        lnotab = self.load()
        return types.CodeType(argcount, nlocals, stacksize, flags, code, consts,
                              names, varnames, filename, name, firstlineno,
                              lnotab, freevars, cellvars)
    dispatch[TYPE_CODE] = load_code

    def load_set(self):
        n = self.r_long()
        args = [self.load() for i in range(n)]
        return set(args)
    dispatch[TYPE_SET] = load_set

    def load_frozenset(self):
        n = self.r_long()
        args = [self.load() for i in range(n)]
        return frozenset(args)
    dispatch[TYPE_FROZENSET] = load_frozenset

# ________________________________________________________________

def _read(self, n):
    pos = self.bufpos
    newpos = pos + n
    ret = self.bufstr[pos : newpos]
    self.bufpos = newpos
    return ret

def _read1(self):
    ret = self.bufstr[self.bufpos]
    self.bufpos += 1
    return ret

def _r_short(self):
    lo = ord(_read1(self))
    hi = ord(_read1(self))
    x = lo | (hi<<8)
    if x & 0x8000:
        x = x - 0x10000
    return x

def _r_long(self):
    # inlined this most common case
    p = self.bufpos
    s = self.bufstr
    a = ord(s[p])
    b = ord(s[p+1])
    c = ord(s[p+2])
    d = ord(s[p+3])
    self.bufpos += 4
    x = a | (b<<8) | (c<<16) | (d<<24)
    if d & 0x80 and x > 0:
        x = -((1L<<32) - x)
        return int(x)
    else:
        return x

def _r_long64(self):
    a = ord(_read1(self))
    b = ord(_read1(self))
    c = ord(_read1(self))
    d = ord(_read1(self))
    e = long(ord(_read1(self)))
    f = long(ord(_read1(self)))
    g = long(ord(_read1(self)))
    h = long(ord(_read1(self)))
    x = a | (b<<8) | (c<<16) | (d<<24)
    x = x | (e<<32) | (f<<40) | (g<<48) | (h<<56)
    if h & 0x80 and x > 0:
        x = -((1L<<64) - x)
    return x

_load_dispatch = {}

class _FastUnmarshaller:

    dispatch = {}

    def __init__(self, buffer):
        self.bufstr = buffer
        self.bufpos = 0
        self._stringtable = []

    def load(self):
        # make flow space happy
        c = '?'
        try:
            c = self.bufstr[self.bufpos]
            self.bufpos += 1
            return _load_dispatch[c](self)
        except KeyError:
            raise ValueError, "bad marshal code: %c (%d)" % (c, ord(c))
        except IndexError:
            raise EOFError

    def load_null(self):
        return _NULL
    dispatch[TYPE_NULL] = load_null

    def load_none(self):
        return None
    dispatch[TYPE_NONE] = load_none

    def load_true(self):
        return True
    dispatch[TYPE_TRUE] = load_true

    def load_false(self):
        return False
    dispatch[TYPE_FALSE] = load_false

    def load_stopiter(self):
        return StopIteration
    dispatch[TYPE_STOPITER] = load_stopiter

    def load_ellipsis(self):
        return Ellipsis
    dispatch[TYPE_ELLIPSIS] = load_ellipsis

    def load_int(self):
        return _r_long(self)
    dispatch[TYPE_INT] = load_int

    def load_int64(self):
        return _r_long64(self)
    dispatch[TYPE_INT64] = load_int64

    def load_long(self):
        size = _r_long(self)
        sign = 1
        if size < 0:
            sign = -1
            size = -size
        x = 0L
        for i in range(size):
            d = _r_short(self)
            x = x | (d<<(i*15L))
        return x * sign
    dispatch[TYPE_LONG] = load_long

    def load_float(self):
        n = ord(_read1(self))
        s = _read(self, n)
        return float(s)
    dispatch[TYPE_FLOAT] = load_float

    def load_complex(self):
        n = ord(_read1(self))
        s = _read(self, n)
        real = float(s)
        n = ord(_read1(self))
        s = _read(self, n)
        imag = float(s)
        return complex(real, imag)
    dispatch[TYPE_COMPLEX] = load_complex

    def load_string(self):
        n = _r_long(self)
        return _read(self, n)
    dispatch[TYPE_STRING] = load_string

    def load_interned(self):
        n = _r_long(self)
        ret = intern(_read(self, n))
        self._stringtable.append(ret)
        return ret
    dispatch[TYPE_INTERNED] = load_interned

    def load_stringref(self):
        n = _r_long(self)
        return self._stringtable[n]
    dispatch[TYPE_STRINGREF] = load_stringref

    def load_unicode(self):
        n = _r_long(self)
        s = _read(self, n)
        ret = s.decode('utf8')
        return ret
    dispatch[TYPE_UNICODE] = load_unicode

    def load_tuple(self):
        return tuple(self.load_list())
    dispatch[TYPE_TUPLE] = load_tuple

    def load_list(self):
        n = _r_long(self)
        list = []
        for i in range(n):
            list.append(self.load())
        return list
    dispatch[TYPE_LIST] = load_list

    def load_dict(self):
        d = {}
        while 1:
            key = self.load()
            if key is _NULL:
                break
            value = self.load()
            d[key] = value
        return d
    dispatch[TYPE_DICT] = load_dict

    def load_code(self):
        argcount = _r_long(self)
        nlocals = _r_long(self)
        stacksize = _r_long(self)
        flags = _r_long(self)
        code = self.load()
        consts = self.load()
        names = self.load()
        varnames = self.load()
        freevars = self.load()
        cellvars = self.load()
        filename = self.load()
        name = self.load()
        firstlineno = _r_long(self)
        lnotab = self.load()
        return types.CodeType(argcount, nlocals, stacksize, flags, code, consts,
                              names, varnames, filename, name, firstlineno,
                              lnotab, freevars, cellvars)
    dispatch[TYPE_CODE] = load_code

    def load_set(self):
        n = _r_long(self)
        args = [self.load() for i in range(n)]
        return set(args)
    dispatch[TYPE_SET] = load_set

    def load_frozenset(self):
        n = _r_long(self)
        args = [self.load() for i in range(n)]
        return frozenset(args)
    dispatch[TYPE_FROZENSET] = load_frozenset

_load_dispatch = _FastUnmarshaller.dispatch

# _________________________________________________________________
#
# user interface

version = 1

def load(f):
    um = _Unmarshaller(f.read)
    return um.load()

def loads(s):
    um = _FastUnmarshaller(s)
    return um.load()
