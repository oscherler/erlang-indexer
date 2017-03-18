-module(indexer).
-include_lib("eunit/include/eunit.hrl").
-export( [ extract_words/1 ] ).

extract_words_test() ->
    [] = extract_words(""),
    [ "foo" ] = extract_words("foo"),
    [ "foo", "bar" ] = extract_words("foo bar"),
    [ "the", "quick", "brown", "fox" ] = extract_words("the “quick” brown fox!").

extract_words( Line ) ->
    Normalise = fun( C ) ->
        case is_letter( C ) of
            true  -> C;
            false -> 0
        end
    end,
    Normalised = lists:map( Normalise, Line ),
    tokenize( Normalised, 0 ).

is_letter_test() ->
    true = is_letter( $F ),
    true = is_letter( $k ),
    false = is_letter( $! ),
    false = is_letter( $  ).

is_letter( C ) when C >= $a andalso C =< $z ->
    true;
is_letter( C ) when C >= $A andalso C =< $Z ->
    true;
is_letter( _ ) ->
    false.

tokenize_test() ->
    [] = tokenize( [], $x ),
    [ "A", "B" ] = tokenize( "AxB", $x ),
    [ "foo", "bar" ] = tokenize( " foo   bar  ", $  ).

tokenize( String, Separator ) ->
    lists:reverse( tokenize( [], String, Separator, [] ) ).

% reached the end of the string, append current token if not empty
tokenize( Current, [], _Separator, Tokens ) ->
    prepend_non_empty_token( Tokens, lists:reverse( Current ) );
% reached separator, start a new token and append current if not empty
tokenize( Current, [ Separator | Cs ], Separator, Tokens ) ->
    tokenize( [], Cs, Separator, prepend_non_empty_token( Tokens, lists:reverse( Current ) ) );
% inside a token, append character to current token
tokenize( Current, [ C | Cs ], Separator, Tokens ) ->
    tokenize( [ C | Current ], Cs, Separator, Tokens ).

prepend_non_empty_token_test() ->
    [] = prepend_non_empty_token( [], [] ),
    [ "foo" ] = prepend_non_empty_token( [ "foo" ], [] ),
    [ "bar" ] = prepend_non_empty_token( [], "bar" ),
    [ "foo", "bar", "baz" ] = prepend_non_empty_token( [ "bar", "baz" ], "foo" ).

prepend_non_empty_token( Tokens, [] ) ->
    Tokens;
prepend_non_empty_token( Tokens, Current ) ->
    [ Current | Tokens ].
