Enhanced Suffix Tree
==================

An enhanced suffix tree than can operate as a sliding window.

The classic Ukkonen's algorithm is implemented, which is maintained in the classic_suffixtree branch.
The implementation is primarily based on this Stack Overflow post.
http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
In the implementation and comment, I tried to abide to the same notation and names with this post.
This implementation has O(1) insert time, so the build time is O(n). It took O(n) space too.

In addition, an enhanced suffix tree is proposed and implemented. Here is a comparison between the classic suffix tree and the enhanced suffix tree.

|                      Classic suffix tree                      |                                                         Enhanced suffix tree                                                        |
|:-------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------------------------:|
|                         singly linked                         |                                                            doubly linked                                                            |
| keeps the sequence of input                                   | keeps the sequence of input and reference to leaf nodes                                                                             |
| O(n) time to build, can build incrementally                   | O(n) time to build, can build incrementally                                                                                         |
| Need a special ending character to do pattern match correctly | Use recursive tree structure to eliminate the restriction of ending character                                                       |
| Cannot remove inserted input                                  | Can remove from the head of the input. Current implementation need O(n) time to remove from head, but in theory it can be O(log(n)) |

Also, a simple regular expression engine that support "+", "*", "?", ".", "|", "()", "\" operators. The regular expression can do pattern match on suffix tree in about O(s) time, where s is number of matches found in the target corpus.

Currently, the data structure has been tested and works correctly. But the code still need heavy refactoring and better documentation. Be prepared to face a lot of dirty code and scattered comment if you decided to use this.
