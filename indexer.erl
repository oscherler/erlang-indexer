-module(indexer).
-include("global.hrl").
-export( [ make_index/1, print_index/1, print_file_index/1 ] ).

% print_index
% print the index by printing each word and its formatted list of occurences

print_index( [ { Word, Occurences } | Es ] ) ->
    io:format( "~s: ~s~n", [ Word, format_occurences( Occurences ) ] ),
    print_index( Es );
print_index( [] ) ->
    ok.

% format_occurences
% make a string of the line ranges for printing

format_occurences( Occurences ) ->
    Map = fun( { L1, L2 } ) ->
        lists:flatten(
            case L1 == L2 of
                true -> io_lib:format( "~b", [ L1 ] );
                _ -> io_lib:format( "~b-~b", [ L1, L2 ] )
            end
        )
    end,
    string:join( lists:map( Map, Occurences ), ", " ).

% print_file_index
% read a file, index it, and print the result

print_file_index( Name ) ->
    print_index(
        make_index(
            index:get_file_contents( Name )
        )
    ).

% make_index
% make and index by adding each line of the input and calling finish_index

make_index( Lines ) ->
    Reduce = fun( Line, { N, CurrentIndex } ) ->
        NewIndex = index_add_line( Line, N, CurrentIndex ),
        { N + 1, NewIndex }
    end,
    { _, Index } = lists:foldl( Reduce, { 1, [] }, Lines ),
    finish_index( Index ).

% finish_index
% process_occurences on every entry of the index

finish_index( Index ) ->
    Map = fun( { Word, Occurences } ) ->
        { Word, process_occurences( Occurences ) }
    end,
    lists:map( Map, Index ).

% process_occurences
% collapse list of line numbers into a list of ranges
% source list is in reversed order, result is in normal order

process_occurences( Lines ) ->
    Reduce = fun
        ( Line, [] ) ->
            [ { Line, Line } ];
        ( Line, [ { L1, L2 } | Es ] ) when L1 == Line + 1 ->
            [ { Line, L2 } | Es ];
        ( Line, [ E | Es ] ) ->
            [ { Line, Line }, E | Es ]
    end,
    lists:foldl( Reduce, [], Lines ).

% index_add_line
% add every word of the given line to the index

index_add_line( Line, LineNumber, Index ) ->
    Reduce = fun( Word, CurrentIndex ) ->
        index_add_word( Word, LineNumber, CurrentIndex )
    end,
    lists:foldl( Reduce, Index, line_words( Line ) ).

% index_add_word
% add or update entry for Word by prepending given line to list of line numbers

index_add_word( Word, LineNumber, Index ) ->
    { Before, Match, After } = index_find( Word, Index ),
    Updated = case Match of
        { Word, LineNumbers } -> { Word, [ LineNumber | LineNumbers ] };
        _ -> { Word, [ LineNumber ] } % null
    end,
    Before ++ [ Updated | After ].

% index_find
% find an entry with { Word, _ } in a sorted list of entries

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
% take unique words from a line of text

line_words( Line ) ->
    util:unique( extract_words( Line ) ).

% extract_words
% tokenize line on consecutive letters

extract_words( Line ) ->
    Normalise = fun( C ) -> lowercase_letter( C ) end,
    Normalised = lists:map( Normalise, Line ),
    tokenize( Normalised, false ).

% is_letter

lowercase_letter( C ) when C >= $a andalso C =< $z ->
    C;
lowercase_letter( C ) when C >= $A andalso C =< $Z ->
    C + $a - $A;
lowercase_letter( _ ) ->
    false.

% tokenize
% make a list of tokens separated by Separator
% ignore empty tokens

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

format_occurences_test() ->
    ?assertEqual( "", format_occurences( [] ) ),
    ?assertEqual( "3", format_occurences( [ { 3, 3 } ] ) ),
    ?assertEqual( "3, 8", format_occurences( [ { 3, 3 }, { 8, 8 } ] ) ),
    ?assertEqual( "3-5", format_occurences( [ { 3, 5 } ] ) ),
    ?assertEqual( "1, 3-5, 6-12", format_occurences( [ { 1, 1 }, { 3, 5 }, { 6, 12 } ] ) ).

process_occurences_test() ->
    ?assertEqual( [], process_occurences( [] ) ),
    ?assertEqual( [ { 42, 42 } ], process_occurences( [ 42 ] ) ),
    ?assertEqual( [ { 21, 21 }, { 42, 42 } ], process_occurences( [ 42, 21 ] ) ),
    ?assertEqual( [ { 3, 5 }, { 7, 7 } ], process_occurences( [ 7, 5, 4, 3 ] ) ),
    ?assertEqual(
        [ { 1, 5 }, { 8, 8 }, { 11, 11 }, { 13, 14 } ],
        process_occurences( [ 14, 13, 11, 8, 5, 4, 3, 2, 1 ] )
    ).

index_add_line_test() ->
    Index = [ { "apple", [] }, { "deer", [ 4 ] }, { "mango", [] } ],

    ?assertEqual(
        [ { "apple", [ 12 ] }, { "deer", [ 12, 4 ] }, { "hotel", [ 12 ] }, { "mango", [] } ],
        index_add_line( "deer apple hotel", 12, Index )
    ),
    ?assertEqual(
        [ { "apple", [ 15 ] }, { "deer", [ 4 ] }, { "hotel", [ 15 ] }, { "mango", [ 15 ] } ],
        index_add_line( "hotel apple hotel mango mango mango", 15, Index )
    ),
    ?assertEqual(
        [ { "apple", [ 13 ] }, { "deer", [ 13, 4 ] }, { "hotel", [ 13 ] }, { "mango", [] } ],
        index_add_line( "-hotel/apple++(deer)", 13, Index )
    ).

index_add_word_test() ->
    Index = [ { "apple", [] }, { "deer", [ 4 ] }, { "mango", [] } ],

    ?assertEqual(
        [ { "apple", [] }, { "deer", [ 8, 4 ] }, { "mango", [] } ],
        index_add_word( "deer", 8, Index )
    ),
    ?assertEqual(
        [ { "apple", [] }, { "deer", [ 4 ] }, { "hotel", [ 11 ] }, { "mango", [] } ],
        index_add_word( "hotel", 11, Index )
    ).

index_find3_empty_test() ->
    ?assertEqual(
        {
            [],    % before
            null,  % match
            []     % after
        },
        index_find(
            "foo", % needle
            [],    % rest of index
            []     % skipped (reversed)
        )
    ).
index_find3_ended_test() ->
    ?assertEqual(
        {
            [      % before
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            null,  % match
            []     % after
        },
        index_find(
            "foo", % needle
            [],    % rest of index
            [      % skipped (reversed)
                { "deer", [ 4 ] },
                { "apple", [] }
            ]
        )
    ).
index_find3_found_end_test() ->
    ?assertEqual(
        {
            [
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            { "mango", [] },
            []
        },
        index_find(
            "mango",
            [
                { "mango", [] }
            ],
            [
                { "deer", [ 4 ] },
                { "apple", [] }
            ]
        )
    ).
index_find3_found_test() ->
    ?assertEqual(
        {
            [
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            { "mango", [] },
            [
                { "river", [] }
            ]
        },
        index_find(
            "mango",
            [
                { "mango", [] },
                { "river", [] }
            ],
            [
                { "deer", [ 4 ] },
                { "apple", [] }
            ]
        )
    ).
index_find3_past_test() ->
    ?assertEqual(
        {
            [
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            null,
            [
                { "mango", [] }
            ]
        },
        index_find(
            "foo",
            [
                { "mango", [] }
            ],
            [
                { "deer", [ 4 ] },
                { "apple", [] }
            ]
        )
    ).

index_find_test() ->
    Index = [ { "apple", [] }, { "deer", [ 4 ] }, { "mango", [] } ],

    ?assertEqual(
        {
            [],
            null,
            []
        },
        index_find( "foo", [] )
    ),
    ?assertEqual(
        {
            [
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            null,
            [
                { "mango", [] }
            ]
        },
        index_find( "foo", Index )
    ),
    ?assertEqual(
        {
            [
                { "apple", [] },
                { "deer", [ 4 ] }
            ],
            null,
            [
                { "mango", [] }
            ]
        },
        index_find( "hotel", Index )
    ),
    ?assertEqual(
        {
            [
                { "apple", [] }
            ],
            { "deer", [ 4 ] },
            [
                { "mango", [] }
            ]
        },
        index_find( "deer", Index )
    ),
    ?assertEqual(
        {
            [],
            { "apple", [] },
            [
                { "deer", [ 4 ] },
                { "mango", [] }
            ]
        },
        index_find( "apple", Index )
    ).

line_words_test() ->
    ?assertEqual( [], line_words("") ),
    ?assertEqual( [ "foo" ], line_words("foo") ),
    ?assertEqual( [ "foo" ], line_words("  foo!") ),
    ?assertEqual( [ "foo", "bar" ], line_words("  foo! -bar") ),
    ?assertEqual( [ "foo", "bar" ], line_words("foo bar bar foo foo") ).

extract_words_test() ->
    ?assertEqual( [], extract_words("") ),
    ?assertEqual( [ "foo" ], extract_words("foo") ),
    ?assertEqual( [ "foo", "bar" ], extract_words("foo bar") ),
    ?assertEqual(
        [ "the", "quick", "brown", "fox" ],
        extract_words("the “quick” brown fox!")
    ).

lowercase_letter_test() ->
    ?assertEqual( $f, lowercase_letter( $F ) ),
    ?assertEqual( $k, lowercase_letter( $k ) ),
    ?assertEqual( false, lowercase_letter( $! ) ),
    ?assertEqual( false, lowercase_letter( $  ) ).

tokenize_test() ->
    ?assertEqual( [], tokenize( [], $x ) ),
    ?assertEqual( [ "A", "B" ], tokenize( "AxB", $x ) ),
    ?assertEqual( [ "foo", "bar" ], tokenize( " foo   bar  ", $  ) ).
