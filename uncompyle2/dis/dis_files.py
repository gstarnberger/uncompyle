import uncompyle2.magics as magics

__all__ = ['by_version', 'by_magic']

_fallback = {
    'EXTENDED_ARG': None,
    'hasfree': [],
    }

class dis(object):
    def __init__(self, version, module):
        self._version = version
        from __builtin__ import __import__
        self._module = __import__('uncompyle2.%s' % module, globals(),
                                  locals(), 'uncompyle2')

    def __getattr__(self, attr):
        try:
            val = self._module.__dict__[attr]
        except KeyError, e:
            if _fallback.has_key(attr):
                val = _fallback[attr]
            else:
                raise e
        return val

by_version = {
    '1.5': dis('1.5', 'dis.dis_15'),
    '1.6': dis('1.6', 'dis.dis_16'),
    '2.0': dis('2.0', 'dis.dis_20'),
    '2.1': dis('2.1', 'dis.dis_21'),
    '2.2': dis('2.2', 'dis.dis_22'),
    '2.3': dis('2.3', 'dis.dis_23'),
    '2.4': dis('2.4', 'dis.dis_24'),
    '2.5': dis('2.5', 'dis.dis_25'),
    '2.6': dis('2.6', 'dis.dis_26'),
    '2.7': dis('2.7', 'dis.dis_27')
}

by_magic = dict( [ (mag, by_version[ver])
                   for mag, ver in magics.versions.iteritems() ] )

if __name__ == '__main__':
    for m, ver in by_magic.items():
        magics.__show(ver, m)
    print by_version['2.2'].hasjrel
