# BloomURL

Tiny bloom filters with URL encoded serialization.

    >>> B = bloomurl:new(50, 0.1). % 10% false-positives at 50 elements
    >>> bloomurl:member(test, B).
    false
    >>> B1 = bloomurl:add(test, B).
    >>> bloomurl:member(test, B1).
    true
    >>> S = bloomurl:serialize(B1).
    <<"240.4.AAAAAAgAAgAAAAAAAAAAAAAAEAAEAAAAAAAAAAAA">>
    >>> B1 =:= bloomurl:deserialize(S).
    true

## Credits

Most of the basic bloom filter code was adapted from
[Christian Lundgren's bloom_filterl](https://github.com/chrisavl/bloom_filterl).


## Copyright

Copyright (c) 2013 Martin Rehfeld. See LICENSE for details.
