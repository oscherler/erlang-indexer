-module(indexer).
-include_lib("eunit/include/eunit.hrl").
-export( [ extract_words/1 ] ).

extract_words_test() ->
    [] = extract_words(""),
    [ "foo" ] = extract_words("foo"),
    [ "foo", "bar" ] = extract_words("foo bar"),
    [ "the", "quick", "brown", "fox" ] = extract_words("the “quick” brown fox!").

extract_words( Line ) ->
    Normalised = lists:map(
        fun( C ) ->
            case is_letter( C ) of
                true -> C;
                false -> 0
            end
        end,
        Line
    ),
    tokenize( Normalised, 0 ).

is_letter_test() ->
    true = is_letter( $F ),
    true = is_letter( $k ),
    false = is_letter( $! ),
    false = is_letter( $  ).

is_letter( C ) ->
    if
        C >= $a andalso C =< $z -> true;
        C >= $A andalso C =< $Z -> true;
        true -> false
    end.

tokenize_test() ->
    [] = tokenize( [], $x ),
    [ "A", "B" ] = tokenize( "AxB", $x ),
    [ "foo", "bar" ] = tokenize( " foo   bar  ", $  ).

tokenize( String, Separator ) ->
    tokenize( [], String, Separator, [] ).

tokenize( [], [], _Separator, Tokens ) ->
    Tokens;
tokenize( Current, [], _Separator, Tokens ) ->
    Tokens ++ [ Current ];
tokenize( [], [ Separator | Cs ], Separator, Tokens ) ->
    tokenize( [], Cs, Separator, Tokens );
tokenize( Current, [ Separator | Cs ], Separator, Tokens ) ->
    tokenize( [], Cs, Separator, Tokens ++ [ Current ] );
tokenize( Current, [ C | Cs ], Separator, Tokens ) ->
    tokenize( Current ++ [ C ], Cs, Separator, Tokens ).
