-module(tree).
-include("global.hrl").
-export( [ add/3, find/2, map/2, walk/2 ] ).

find( Key, { Key, Data, _, _ } ) ->
    Data;
find( Key, { K, _D, Before, _After } ) when Key < K ->
    find( Key, Before );
find( Key, { K, _D, _Before, After } ) when Key > K ->
    find( Key, After );
find( _Key, _ ) ->
    none.

add( Key, Data, none ) ->
    { Key, Data, none, none };
add( Key, Data, { Key, _D, Before, After } ) ->
    { Key, Data, Before, After };
add( Key, Data, { K, D, Before, After } ) when Key < K ->
    { K, D, add( Key, Data, Before ), After };
add( Key, Data, { K, D, Before, After } ) when Key > K ->
    { K, D, Before, add( Key, Data, After ) }.

map( _Fun, none ) ->
    none;
map( Fun, { Key, Data, Before, After } ) ->
    { Key, Fun( Data ), map( Fun, Before ), map( Fun, After ) }.

walk( _Fun, none ) ->
    ok;
walk( Fun, { Key, Data, Before, After } ) ->
    walk( Fun, Before ),
    Fun( Key, Data ),
    walk( Fun, After ).

add_empty_test() ->
    ?assertEqual(
        { "key", "data", none, none },
        add( "key", "data", none )
    ).

add_before_test() ->
    Tree = { "key", "data", none, none },
    ?assertEqual(
        {
            "key", "data",
            { "gig", "gata", none, none },
            none
        },
        add( "gig", "gata", Tree )
    ).

add_after_test() ->
    Tree = { "key", "data", none, none },
    ?assertEqual(
        {
            "key", "data",
            none,
            { "rig", "rata", none, none }
        },
        add( "rig", "rata", Tree )
    ).

replace_test() ->
    Tree = { "key", "data", none, none },
    ?assertEqual(
        { "key", "rata", none, none },
        add( "key", "rata", Tree )
    ).

add_before_l2_test() ->
    Tree = {
        "key", "data",
        { "gig", "gata", none, none },
        none
    },
    ?assertEqual(
        {
            "key", "data",
            {
                "gig", "gata",
                { "fig", "fata", none, none },
                none
            },
            none
        },
        add( "fig", "fata", Tree )
    ).

add_after_l2_test() ->
    Tree = {
        "key", "data",
        none,
        { "rig", "rata", none, none }
    },
    ?assertEqual(
        {
            "key", "data",
            none,
            {
                "rig", "rata",
                none,
                { "tip", "tata", none, none }
            }
        },
        add( "tip", "tata", Tree )
    ).

add_before_after_before_test() ->
    Tree = {
        "key", "data",
        {
            "bib", "bata",
            none,
            { "hat", "hata", none, none }
        },
        { "rig", "rata", none, none }
    },
    ?assertEqual(
        {
            "key", "data",
            {
                "bib", "bata",
                none,
                {
                    "hat", "hata",
                    { "fat", "feta", none, none },
                    none
                }
            },
            { "rig", "rata", none, none }
        },
        add( "fat", "feta", Tree )
    ).

add_after_before_before_after_test() ->
    Tree = {
        "key", "data",
        { "bib", "bata", none, none },
        {
            "rig", "rata",
            {
                "pit", "pata",
                { "nit", "nata", none, none },
                none
            },
            none
        }
    },
    ?assertEqual(
        {
            "key", "data",
            { "bib", "bata", none, none },
            {
                "rig", "rata",
                {
                    "pit", "pata",
                    {
                        "nit", "nata",
                        none,
                        { "oil", "ota", none, none }
                    },
                    none
                },
                none
            }
        },
        add( "oil", "ota", Tree )
    ).

replace_deep_test() ->
    Tree = {
        "key", "data",
        { "bib", "bata", none, none },
        {
            "rig", "rata",
            {
                "pit", "pata",
                {
                    "nit", "nata",
                    none,
                    { "oil", "ota", none, none }
                },
                none
            },
            none
        }
    },
    NewTree = {
        "key", "kata",
        { "bib", "better", none, none },
        {
            "rig", "rata",
            {
                "pit", "pattern",
                {
                    "nit", "neater",
                    none,
                    { "oil", "otter", none, none }
                },
                none
            },
            none
        }
    },
    ?assertEqual(
        NewTree,
        add(
            "oil", "otter",
            add(
                "pit", "pattern",
                add(
                    "bib", "better",
                    add(
                        "nit", "neater",
                        add( "key", "kata", Tree )
                    )
                )
            )
        )
    ),
    ?assertEqual(
        NewTree,
        add(
            "key", "kata",
            add(
                "bib", "better",
                add(
                    "oil", "otter",
                    add(
                        "pit", "pattern",
                        add( "nit", "neater", Tree )
                    )
                )
            )
        )
    ),
    ?assertEqual(
        NewTree,
        add(
            "nit", "neater",
            add(
                "oil", "otter",
                add(
                    "bib", "better",
                    add(
                        "key", "kata",
                        add( "pit", "pattern", Tree )
                    )
                )
            )
        )
    ).

find_test() ->
    Tree = {
        "key", "data",
        { "bib", "bata", none, none },
        {
            "rig", "rata",
            {
                "pit", "pata",
                {
                    "nit", "nata",
                    none,
                    { "oil", "ota", none, none }
                },
                none
            },
            none
        }
    },
    ?assertEqual( "data", find( "key", Tree ) ),
    ?assertEqual( "bata", find( "bib", Tree ) ),
    ?assertEqual( "rata", find( "rig", Tree ) ),
    ?assertEqual( "pata", find( "pit", Tree ) ),
    ?assertEqual( "nata", find( "nit", Tree ) ),
    ?assertEqual( "ota", find( "oil", Tree ) ),
    ?assertEqual( none, find( "foo", Tree ) ).

map_test() ->
    Tree = {
        "key", "data",
        { "bib", "bata", none, none },
        {
            "rig", "rata",
            {
                "pit", "pata",
                {
                    "nit", "nata",
                    none,
                    { "oil", "ota", none, none }
                },
                none
            },
            none
        }
    },
    Reversed = {
        "key", "atad",
        { "bib", "atab", none, none },
        {
            "rig", "atar",
            {
                "pit", "atap",
                {
                    "nit", "atan",
                    none,
                    { "oil", "ato", none, none }
                },
                none
            },
            none
        }
    },
    ?assertEqual(
        Reversed,
        map(
            fun( D ) -> lists:reverse( D ) end,
            Tree
        )
    ).
