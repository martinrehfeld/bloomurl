-module(bloomurl).

-export([ new/2
        , add/2
        , member/2
        , serialize/1
        , deserialize/1]).

-import(math, [log/1, pow/2]).


-define(SALT, ?MODULE).

-define(l2b(L), list_to_binary(L)).
-define(b2l(B), binary_to_list(B)).
-define(b2i(B), list_to_integer(binary_to_list(B))).


-type bitsize() :: pos_integer().
-type bits_per_key() :: pos_integer().
-type bloom_filter() :: {binary(), bitsize(), bits_per_key()}.


%% ===================================================================
%% Public functions
%% ===================================================================

-spec new(pos_integer(), float()) -> bloom_filter().
new(_, ErrorRate) when ErrorRate =< 0; ErrorRate > 1.0 ->
    error(badarg);
new(Size, ErrorRate) ->
    new(layout(Size, ErrorRate)).

new({M, _}) when M > 134217727 -> % always limit to 32-bit arch max
    error(badarg);
new({M, K}) ->
    {new_bitarray(M, false), M, K}.


-spec add(term(), bloom_filter()) -> bloom_filter().
add(Key, BloomFilter) ->
    lists:foldl(fun (Idx, BF) -> set_bit(Idx, BF) end,
                BloomFilter,
                indices(Key, BloomFilter)).


-spec member(term(), bloom_filter()) -> boolean().
member(Key, BloomFilter) ->
    lists:all(fun (Idx) -> is_bit_set(Idx, BloomFilter) end,
              indices(Key, BloomFilter)).


-spec serialize(bloom_filter()) -> binary().
serialize({Bitarray, Bitsize, BitsPerKey}) ->
    list_to_binary(io_lib:format("~B.~B.~s", [Bitsize,
                                              BitsPerKey,
                                              base64_url_encode(Bitarray)])).

-spec deserialize(binary()) -> bloom_filter().
deserialize(SerializedBloomFilter) ->
    [M, K, B] = binary:split(SerializedBloomFilter, <<$.>>, [global]),
    Bitarray = base64_url_decode(B),
    {Bitarray, ?b2i(M), ?b2i(K)}.


%% ===================================================================
%% Internal functions
%% ===================================================================

new_bitarray(Size, Default) ->
    hipe_bifs:bitarray(Size, Default).


set_bit(Idx, {Array, M, K}) when Idx < M ->
    {hipe_bifs:bitarray_update(Array, Idx, true), M, K}.


is_bit_set(Idx, {Array, M, _K}) when Idx < M ->
    hipe_bifs:bitarray_sub(Array, Idx).


layout(N, P) ->
    M = trunc(-(N * log(P))/pow(log(2),2) + 1),
    K = trunc(M * log(2) / N + 1),
    {M, K}.


indices(Key, {_Array, M, K}) ->
    lists:map(fun (I) -> double_hash(Key, I) rem M end,
              lists:seq(0, K - 1)).


double_hash(Key, I) -> hash1(Key) + I * hash2(Key).
hash1(Key) -> erlang:phash2(Key).
hash2(Key) -> erlang:phash2({?SALT, Key}).


base64_url_decode(Content) when is_binary(Content) ->
    base64_url_decode(?b2l(Content));
base64_url_decode(Content) when is_list(Content) ->
    Base64Unpadded = [base64_url_decode_char(Char) || Char <- Content, Char =/= $\n],
    Base64Padded = base64_pad(Base64Unpadded),
    base64:decode(?l2b(Base64Padded)).


base64_url_encode(Content) when is_binary(Content) ->
    base64_url_encode(?b2l(Content));
base64_url_encode(Content) when is_list(Content) ->
    Base64 = base64:encode_to_string(Content),
    ?l2b([base64_url_encode_char(Char) || Char <- Base64, Char =/= $=]).


base64_url_decode_char($-) ->
    $+;
base64_url_decode_char($_) ->
    $/;
base64_url_decode_char(Char) ->
    Char.


base64_url_encode_char($+) ->
    $-;
base64_url_encode_char($/) ->
    $_;
base64_url_encode_char(Char) ->
    Char.


base64_pad(String) ->
    Length = length(String),

    Rem = Length rem 4,
    ToPad = case Rem of
                0 -> 0;
                N -> 4 - N
            end,
    string:left(String, Length + ToPad, $=).
