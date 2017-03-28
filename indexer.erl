-module(indexer).
-include("global.hrl").
-export( [ extract_words/1 ] ).

% index_add_word

index_add_word( Word, Line, Index ) ->
    { Before, Match, After } = index_find( Word, Index ),
    Updated = case Match of
        { Word, Lines } -> { Word, [ Line | Lines ] };
        _ -> { Word, [ Line ] } % null
    end,
    Before ++ [ Updated | After ].

% index_find

index_find( Word, Index ) ->
    index_find( Word, Index, [] ).

index_find( _Word, [], Before ) ->
    { lists:reverse( Before ), null, [] };
index_find( Word, [ Entry = { Word, _ } | Rest ], Before ) ->
    { lists:reverse( Before ), Entry, Rest };
index_find( Word, [ Entry = { Current, _ } | Rest ], Before ) when Word < Current ->
    { lists:reverse( Before ), null, [ Entry | Rest ] };
index_find( Word, [ Entry = { _Current, _ } | Rest ], Before ) ->
    index_find( Word, Rest, [ Entry | Before ] ).

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

index_add_word_test() ->
    Index = [ { "apple", [] }, { "deer", [ 4 ] }, { "mango", [] } ],
    FoundIndex = [ { "apple", [] }, { "deer", [ 8, 4 ] }, { "mango", [] } ],
    NewIndex = [ { "apple", [] }, { "deer", [ 4 ] }, { "hotel", [ 11 ] }, { "mango", [] } ],

    FoundIndex = index_add_word( "deer", 8, Index ),
    NewIndex = index_add_word( "hotel", 11, Index ).

index_find3_empty_test() ->
    {
        [],    % before
        null,  % match
        []     % after
    } = index_find(
        "foo", % needle
        [],    % rest of index
        []     % skipped (reversed)
    ).
index_find3_ended_test() ->
    {
        [      % before
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        null,  % match
        []     % after
    } = index_find(
        "foo", % needle
        [],    % rest of index
        [      % skipped (reversed)
            { "deer", [ 4 ] },
            { "apple", [] }
        ]
    ).
index_find3_found_end_test() ->
    {
        [
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        { "mango", [] },
        []
    } = index_find(
        "mango",
        [
            { "mango", [] }
        ],
        [
            { "deer", [ 4 ] },
            { "apple", [] }
        ]
    ).
index_find3_found_test() ->
    {
        [
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        { "mango", [] },
        [
            { "river", [] }
        ]
    } = index_find(
        "mango",
        [
            { "mango", [] },
            { "river", [] }
        ],
        [
            { "deer", [ 4 ] },
            { "apple", [] }
        ]
    ).
index_find3_past_test() ->
    {
        [
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        null,
        [
            { "mango", [] }
        ]
    } = index_find(
        "foo",
        [
            { "mango", [] }
        ],
        [
            { "deer", [ 4 ] },
            { "apple", [] }
        ]
    ).

index_find_test() ->
    Index = [ { "apple", [] }, { "deer", [ 4 ] }, { "mango", [] } ],

    {
        [],
        null,
        []
    } = index_find( "foo", [] ),
    {
        [
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        null,
        [
            { "mango", [] }
        ]
    } = index_find( "foo", Index ),
    {
        [
            { "apple", [] },
            { "deer", [ 4 ] }
        ],
        null,
        [
            { "mango", [] }
        ]
    } = index_find( "hotel", Index ),
    {
        [
            { "apple", [] }
        ],
        { "deer", [ 4 ] },
        [
            { "mango", [] }
        ]
    } = index_find( "deer", Index ),
    {
        [],
        { "apple", [] },
        [
            { "deer", [ 4 ] },
            { "mango", [] }
        ]
    } = index_find( "apple", Index ).

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
