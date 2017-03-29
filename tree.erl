-module(tree).
-include("global.hrl").
-export( [ tree_add/3 ] ).

tree_find( Key, { Key, Data, _, _ } ) ->
    Data;
tree_find( Key, { K, _D, Before, _After } ) when Key < K ->
    tree_find( Key, Before );
tree_find( Key, { K, _D, _Before, After } ) when Key > K ->
    tree_find( Key, After );
tree_find( _Key, _ ) ->
    none.

tree_add( Key, Data, none ) ->
    { Key, Data, none, none };
tree_add( Key, Data, { Key, _D, Before, After } ) ->
    { Key, Data, Before, After };
tree_add( Key, Data, { K, D, Before, After } ) when Key < K ->
    { K, D, tree_add( Key, Data, Before ), After };
tree_add( Key, Data, { K, D, Before, After } ) when Key > K ->
    { K, D, Before, tree_add( Key, Data, After ) }.

tree_add_empty_test() ->
    { "key", "data", none, none } = tree_add( "key", "data", none ).

tree_add_before_test() ->
    Tree = { "key", "data", none, none },
    {
        "key", "data",
        { "gig", "gata", none, none },
        none
    } = tree_add( "gig", "gata", Tree ).

tree_add_after_test() ->
    Tree = { "key", "data", none, none },
    {
        "key", "data",
        none,
        { "rig", "rata", none, none }
    } = tree_add( "rig", "rata", Tree ).

tree_replace_test() ->
    Tree = { "key", "data", none, none },
    { "key", "rata", none, none } = tree_add( "key", "rata", Tree ).

tree_add_before_l2_test() ->
    Tree = {
        "key", "data",
        { "gig", "gata", none, none },
        none
    },
    {
        "key", "data",
        {
            "gig", "gata",
            { "fig", "fata", none, none },
            none
        },
        none
    } = tree_add( "fig", "fata", Tree ).

tree_add_after_l2_test() ->
    Tree = {
        "key", "data",
        none,
        { "rig", "rata", none, none }
    },
    {
        "key", "data",
        none,
        {
            "rig", "rata",
            none,
            { "tip", "tata", none, none }
        }
    } = tree_add( "tip", "tata", Tree ).

tree_add_before_after_before_test() ->
    Tree = {
        "key", "data",
        {
            "bib", "bata",
            none,
            { "hat", "hata", none, none }
        },
        { "rig", "rata", none, none }
    },
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
    } = tree_add( "fat", "feta", Tree ).

tree_add_after_before_before_after_test() ->
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
    } = tree_add( "oil", "ota", Tree ).

tree_replace_deep_test() ->
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
    NewTree = tree_add(
        "oil", "otter",
        tree_add(
            "pit", "pattern",
            tree_add(
                "bib", "better",
                tree_add(
                    "nit", "neater",
                    tree_add( "key", "kata", Tree )
                )
            )
        )
    ),
    NewTree = tree_add(
        "key", "kata",
        tree_add(
            "bib", "better",
            tree_add(
                "oil", "otter",
                tree_add(
                    "pit", "pattern",
                    tree_add( "nit", "neater", Tree )
                )
            )
        )
    ),
    NewTree = tree_add(
        "nit", "neater",
        tree_add(
            "oil", "otter",
            tree_add(
                "bib", "better",
                tree_add(
                    "key", "kata",
                    tree_add( "pit", "pattern", Tree )
                )
            )
        )
    ).

tree_find_test() ->
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
    ?assertEqual( "data", tree_find( "key", Tree ) ),
    ?assertEqual( "bata", tree_find( "bib", Tree ) ),
    ?assertEqual( "rata", tree_find( "rig", Tree ) ),
    ?assertEqual( "pata", tree_find( "pit", Tree ) ),
    ?assertEqual( "nata", tree_find( "nit", Tree ) ),
    ?assertEqual( "ota", tree_find( "oil", Tree ) ),
    ?assertEqual( none, tree_find( "foo", Tree ) ).
