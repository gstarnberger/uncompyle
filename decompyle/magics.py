import struct

__all__ = ['magics', 'versions']

def __build_magic(magic):
    return struct.pack('Hcc', magic, '\r', '\n')

def __by_version(magics):
    by_version = {}
    for m, v in magics.items():
        by_version[v] = m
    return by_version

versions = {
    # magic, version
    __build_magic(20121): '1.5',
    __build_magic(50428): '1.6',
    __build_magic(50823): '2.0',
    __build_magic(60202): '2.1',
    __build_magic(60717): '2.2',
    __build_magic(62011): '2.3',
}

magics = __by_version(versions)

def __show(text, magic):
    print text, struct.unpack('BBBB', magic), \
          struct.unpack('HBB', magic)

def test():
    import imp
    magic_20 = by_version['2.0']
    current = imp.get_magic()
    current_version = magics[current]
    magic_current = by_version[ current_version ]
    print type(magic_20), len(magic_20), repr(magic_20)
    print
    print 'This Python interpreter has version', current_version
    __show('imp.get_magic():\t', current),
    __show('magic[current_version]:\t', magic_current)
    __show('magic_20:\t\t', magic_20)
    
if __name__ == '__main__':
    test()
