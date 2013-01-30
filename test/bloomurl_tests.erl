-module(bloomurl_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(IT, bloomurl).
-define(URLSAFE_ALPHABET, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.").


proper_test_() ->
    [ ?_test(test_property(fun prop_add_element/0))
    , ?_test(test_property(fun prop_serialization/0))
    ].


prop_add_element() ->
    ?FORALL({Element, Previous, Size}, {term(), list(term()), pos_integer()},
        begin
                Filter = lists:foldl(fun (T, F) -> ?IT:add(T, F) end,
                                     ?IT:new(Size, random:uniform()),
                                     [Element | Previous]),
                ?IT:member(Element, Filter)
        end).

prop_serialization() ->
    ?FORALL({Element, Previous, Size}, {term(), list(term()), pos_integer()},
        begin
                Filter = lists:foldl(fun (T, F) -> ?IT:add(T, F) end,
                                     ?IT:new(Size, random:uniform()),
                                     [Element | Previous]),
                Serialized = ?IT:serialize(Filter),
                true = is_urlsafe(Serialized),
                Deserialized = ?IT:deserialize(Serialized),
                Deserialized =:= Filter
        end).


error_rate_test() ->
    Previous = ordsets:from_list([random:uniform() || _ <- lists:seq(1, 1000)]),
    ErrorRate = 0.0001,
    Size = trunc(1.05 * length(Previous)), % Simulate bloom filter at 95% capacity
    Filter = lists:foldl(fun (T, F) -> ?IT:add(T, F) end,
                         ?IT:new(Size, ErrorRate),
                         Previous),
    Other = [random:uniform() || _ <- lists:seq(1, 100000)],
    Errors = lists:sum(lists:map(
                fun (E) ->
                        case (not ordsets:is_element(E, Previous))
                               andalso ?IT:member(E, Filter) of
                            true  -> 1;
                            false -> 0
                        end
                end, Other)),
    ?assert(Errors / length(Other) =< ErrorRate).


serialization_fixture_test() ->
    Filter = lists:foldl(fun (T, F) -> ?IT:add(T, F) end,
                         ?IT:new(50, 0.1),
                         [a, b, c, d, e, f, g]),
    ?assertEqual(<<"240.4.BAQAQCAAAAAAAAAA_qoOQQAAAAAAAAAQQAAAIGQA">>,
        ?IT:serialize(Filter)).


deserialization_fixture_test() ->
    Filter = lists:foldl(fun (T, F) -> ?IT:add(T, F) end,
                         ?IT:new(50, 0.1),
                         [a, b, c, d, e, f, g]),
    ?assertEqual(Filter,
        ?IT:deserialize(<<"240.4.BAQAQCAAAAAAAAAA_qoOQQAAAAAAAAAQQAAAIGQA">>)).


%%
%% HELPERS
%%

test_property(P) ->
    ?assert(proper:quickcheck(P())).


is_urlsafe(B) when is_binary(B) ->
    is_urlsafe(binary_to_list(B));
is_urlsafe(S) when is_list(S) ->
    lists:all(fun (C) -> lists:member(C, ?URLSAFE_ALPHABET) end, S).
