-module(indexer).
-include("global.hrl").
-export( [ extract_words/1 ] ).

% line_words

line_words( Line ) ->
    util:unique( extract_words( Line ) ).

% extract_words

extract_words( Line ) ->
    Normalise = fun( C ) ->
        case is_letter( C ) of
            true  -> C;
            false -> 0
        end
    end,
    Normalised = lists:map( Normalise, Line ),
    tokenize( Normalised, 0 ).

% is_letter

is_letter( C ) when C >= $a andalso C =< $z ->
    true;
is_letter( C ) when C >= $A andalso C =< $Z ->
    true;
is_letter( _ ) ->
    false.

% tokenize

tokenize( String, Separator ) ->
    lists:reverse( tokenize( [], String, Separator, [] ) ).

% reached the end of the string, append current token if not empty
tokenize( Current, [], _Separator, Tokens ) ->
    util:prepend_not_empty( Tokens, lists:reverse( Current ) );
% reached separator, start a new token and append current if not empty
tokenize( Current, [ Separator | Cs ], Separator, Tokens ) ->
    tokenize( [], Cs, Separator, util:prepend_not_empty( Tokens, lists:reverse( Current ) ) );
% inside a token, append character to current token
tokenize( Current, [ C | Cs ], Separator, Tokens ) ->
    tokenize( [ C | Current ], Cs, Separator, Tokens ).

% tests

line_words_test() ->
    [] = line_words(""),
    [ "foo" ] = line_words("foo"),
    [ "foo" ] = line_words("  foo!"),
    [ "foo", "bar" ] = line_words("  foo! -bar"),
    [ "foo", "bar" ] = line_words("foo bar bar foo foo").

extract_words_test() ->
    [] = extract_words(""),
    [ "foo" ] = extract_words("foo"),
    [ "foo", "bar" ] = extract_words("foo bar"),
    [ "the", "quick", "brown", "fox" ] = extract_words("the “quick” brown fox!").

is_letter_test() ->
    true = is_letter( $F ),
    true = is_letter( $k ),
    false = is_letter( $! ),
    false = is_letter( $  ).

tokenize_test() ->
    [] = tokenize( [], $x ),
    [ "A", "B" ] = tokenize( "AxB", $x ),
    [ "foo", "bar" ] = tokenize( " foo   bar  ", $  ).
