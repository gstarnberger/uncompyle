import magics

__all__ = ['by_version', 'by_magic']

by_version = {
    '1.5': 'marshal_20',
    '1.6': 'marshal_20',
    '2.0': 'marshal_20',
    '2.1': 'marshal_21',
    '2.2': 'marshal_22',
    '2.3': 'marshal_23',
    '2.4': 'marshal_24',
    '2.5': 'marshal_25',
    '2.6': 'marshal_26'
}

by_magic = dict( [ (mag, by_version[ver])
                   for mag, ver in magics.versions.iteritems() ] )

def import_(module=None,version=None,magic=None):
    if module:    pass
    elif version: module = by_version[version]
    elif magic:   module = by_magic[magic]
    else:
        raise 'at least one argument is required'
    from __builtin__ import __import__
    if module == 'marshal':
        # use current version's 'marshal' module
        return __import__('marshal', globals(), locals())
    else:
        return __import__('unpyc.%s' % module, globals(),
                          locals(), 'unpyc')

