-module(util).
-include("global.hrl").
-export( [ unique/1, prepend_not_empty/2 ] ).

% unique: keep first occurrence of each element
unique( List ) ->
    lists:reverse( unique_reverse( List, [] ) ).

% unique and reverse: keep first occurrence of each element, return a reverses result
unique_reverse( [], Uniques ) ->
    Uniques;
unique_reverse( [ X | Xs ], Uniques ) ->
    NewUniques = case lists:member( X, Uniques ) of
        true -> Uniques;
        false -> [ X | Uniques ]
    end,
    unique_reverse( Xs, NewUniques ).

% prepend token if it’s not empty
prepend_not_empty( Tokens, "" ) ->
    Tokens;
prepend_not_empty( Tokens, Current ) ->
    [ Current | Tokens ].

% tests

unique_test() ->
    ?assertEqual( [], unique( [] ) ),
    ?assertEqual( [ 7, 3, 8, 5 ], unique( [ 7, 3, 8, 5 ] ) ),
    ?assertEqual( [ 2 ], unique( [ 2, 2 ] ) ),
    ?assertEqual( [ 7, 3, 8, 5 ], unique( [ 7, 3, 8, 7, 5, 3 ] ) ).

prepend_not_empty_test() ->
    ?assertEqual( [], prepend_not_empty( [], "" ) ),
    ?assertEqual( [ "foo" ], prepend_not_empty( [ "foo" ], "" ) ),
    ?assertEqual( [ "bar" ], prepend_not_empty( [], "bar" ) ),
    ?assertEqual( [ "foo", "bar", "baz" ], prepend_not_empty( [ "bar", "baz" ], "foo" ) ).
