-module(indexer).
-include("global.hrl").
-export( [ make_index/1, print_index/1, print_file_index/1 ] ).

% print_index
% print the index by printing each word and its formatted list of occurences

print_index( Index ) ->
    tree:walk(
        fun( Word, Occurences ) ->
            io:format( "~s: ~s~n", [ Word, format_occurences( Occurences ) ] )
        end,
        Index
    ).

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
    { _, Index } = lists:foldl( Reduce, { 1, none }, Lines ),
    finish_index( Index ).

% finish_index
% process_occurences on every entry of the index

finish_index( Index ) ->
    Map = fun( Occurences ) ->
        process_occurences( Occurences )
    end,
    tree:map( Map, Index ).

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
    LineNumbers = tree:find( Word, Index ),
    Updated = case LineNumbers of
        none -> [ LineNumber ];
        _ -> [ LineNumber | LineNumbers ]
    end,
    tree:add( Word, Updated, Index ).

% line_words
% take unique words from a line of text

line_words( Line ) ->
    util:unique(
        lists:filter(
            fun( Word ) ->
                length( Word ) > 2
            end,
            extract_words( Line )
        )
    ).

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

make_index_test() ->
    Lines = [
        "apple  banana fig",
        "carrot date carrot fig",
        "+fig%gumbo-banana"
    ],
    Index = make_index( Lines ),
    ?assertEqual( [ { 1, 1 } ], tree:find( "apple", Index ) ),
    ?assertEqual( [ { 1, 1 }, { 3, 3 } ], tree:find( "banana", Index ) ),
    ?assertEqual( [ { 2, 2 } ], tree:find( "carrot", Index ) ),
    ?assertEqual( [ { 2, 2 } ], tree:find( "date", Index ) ),
    ?assertEqual( [ { 1, 3 } ], tree:find( "fig", Index ) ),
    ?assertEqual( [ { 3, 3 } ], tree:find( "gumbo", Index ) ).

finish_index_test() ->
    Index =
        tree:add( "apple", [ 42, 38 ],
            tree:add( "banana", [ 5, 4, 3 ],
                tree:add( "carrot", [ 42, 38, 12, 11, 5, 4, 3 ],
                    none
                )
            )
        ),
    Finished = finish_index( Index ),

    ?assertEqual( [ { 38, 38 }, { 42, 42 } ], tree:find( "apple", Finished ) ),
    ?assertEqual( [ { 3, 5 } ], tree:find( "banana", Finished ) ),
    ?assertEqual( [ { 3, 5 }, { 11, 12 }, { 38, 38 }, {42, 42 } ], tree:find( "carrot", Finished ) ).

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
    Index =
        tree:add( "apple", [],
            tree:add( "deer", [ 4 ],
                tree:add( "mango", [],
                    none
                )
            )
        ),

    I1 = index_add_line( "deer apple a so rad hotel", 12, Index ),
    ?assertEqual( [ 12 ], tree:find( "apple", I1 ) ),
    ?assertEqual( [ 12, 4 ], tree:find( "deer", I1 ) ),
    ?assertEqual( [ 12 ], tree:find( "hotel", I1 ) ),
    ?assertEqual( [], tree:find( "mango", I1 ) ),
    ?assertEqual( none, tree:find( "a", I1 ) ),
    ?assertEqual( none, tree:find( "so", I1 ) ),
    ?assertEqual( [ 12 ], tree:find( "rad", I1 ) ),

    I2 = index_add_line( "hotel apple hotel mango mango mango", 15, Index ),
    ?assertEqual( [ 15 ], tree:find( "apple", I2 ) ),
    ?assertEqual( [ 4 ], tree:find( "deer", I2 ) ),
    ?assertEqual( [ 15 ], tree:find( "hotel", I2 ) ),
    ?assertEqual( [ 15 ], tree:find( "mango", I2 ) ),

    I3 = index_add_line( "-hotel/apple++(deer)", 13, Index ),
    ?assertEqual( [ 13 ], tree:find( "apple", I3 ) ),
    ?assertEqual( [ 13, 4 ], tree:find( "deer", I3 ) ),
    ?assertEqual( [ 13 ], tree:find( "hotel", I3 ) ),
    ?assertEqual( [], tree:find( "mango", I3 ) ).

index_add_word_test() ->
    Index =
        tree:add( "apple", [],
            tree:add( "deer", [ 4 ],
                tree:add( "mango", [],
                    none
                )
            )
        ),

    ?assertEqual(
        [ 8, 4 ],
        tree:find( "deer", index_add_word( "deer", 8, Index ) )
    ),

    index_add_word( "hotel", 11, Index ),
    ?assertEqual(
        [ 11 ],
        tree:find( "hotel", index_add_word( "hotel", 11, Index ) )
    ).

line_words_test() ->
    ?assertEqual( [], line_words("") ),
    ?assertEqual( [ "foo" ], line_words("foo") ),
    ?assertEqual( [ "foo" ], line_words("  foo!") ),
    ?assertEqual( [ "foo", "bar" ], line_words("  foo! -bar") ),
    ?assertEqual( [ "foo", "bar" ], line_words("foo bar bar foo foo") ),
    ?assertEqual( [ "big", "blue", "plane" ], line_words("a so big blue plane") ).

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
