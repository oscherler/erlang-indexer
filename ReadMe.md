# indexer

Index a file in Erlang.

* Compile and run tests with `make`;
* Run on the provided sample text files with `make gettysburg` and `make dickens`.

Sample output:

    above: 16
    add: 16
    advanced: 20
    ago: 1
    all: 3
    altogether: 10
    and: 1, 3, 6, 10, 15, 27
    any: 6
    are: 3, 5, 7
    battle: 7
    before: 22
    ...

In the proposed refinements, I skipped:

* removing common words;
* normalising so that common endings, plurals etc. are identified;
* I also didn’t bother supporting languages other than English (although it’s a pet peeve of mine);

because those are things that are well done by dedicated libraries and I didn’t feel it added anything to the exercise, which is about learning Erlang.

In this course, I usually don’t use library functions until I had a go at implementing them myself, but here I made an exception for `string:join` and `lists:flatten`, that I used for printing the index.

I started out with using a list for the index, in which I stored tuples with the word and the line occurrences. From the beginning I kept the list sorted, because it’s more efficient when you need to insert or update a word to walk a sorted list until the expected position of the word you are looking for (I didn’t bisect because I don’t think it’s efficient on a linked list). I first build the index by making a simple list of the line occurrences for each word (in reversed order as I add to the head). Then I walk the index and process the list of lines to make ranges.

Later I replaced the list with a binary tree, defined in `tree.erl`. I was amazed at how easy and elegant it was to implement. The only hard part was to not make mistakes when writing the trees in the test cases.

For the tests I took the simple path. I know that you usually make one test per case and not large tests with several cases, and that there are mechanisms to make it easier, but I was mostly looking at avoiding regressions, so I didn’t bother.
