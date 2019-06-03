# Redex-Explanation

This project provides an explanation to the reduction sequence of a lambda expression. A lambda expression may have multiple redexes and any redex may be choosen in order to reduce that lambda expression. If no order is choosen it reduces as a graph with an individual expression as a node and all the expressions it can reduce to as its children. This reduction happens in the Reduce module.

The project also contains modules such as Normal and Applicative to define the reduction sequence for Normal order and Applicative order.
It is extensibility in the terms of defining a new order. A template to define a new reduction sequence order by a user is provided in the Order module.

Other modules:
Expr - To define the syntax for Lambda expressions.
Graph - To define the structure of Graphs. Nodes store expressions and Edges between 2 nodes store the relation between the two expressions represented by the nodes(Beta-reduction and Alpha-equivalence)
Eval - To evaluate a single expression and output the next expressions it reduces to.
Capture - To check for Variable capture and avoid it.
Alpha - To check if two nodes in the graph are alpha confluent and produce unidirectional edges between them to relate them. This is more useful when we do not define a reduction sequence and produce a N-degree graph.
Reduce - To produce a graph as output. It recurses on each eval sequence. Eval produces one step of the recursion that is a node and its children. Reduce recurses on it to create a complete sequence(graph in case of no reduction order)

To run this project. Commands:
ghci Views.hs(This will load the rest of modules)
There are 4 expression examples in the Expr module (e,e1,e2,e3)
To give out a complete graph:
>>views e
>>views e1
>>views e2
>>views e3

To give out sequence through Normal order :
>>evaluate e normalRedex
>>evaluate e1 normalRedex
>>evaluate e2 normalRedex
>>evaluate e3 normalRedex

To give out sequence through Applicative order :
>>evaluate e applicativeRedex
>>evaluate e1 applicativeRedex
>>evaluate e2 applicativeRedex
>>evaluate e3 applicativeRedex

--For views on output
To find a context, i.e., what are the children of a node
>>let g = views e3
>>context 5 Down g--5 is the index to a node

To find the nodes that reduces to a particular node
>>let g = views e3
>>context 5 Up g

To check for alphaconfluent nodes
>>let g = views e3
>>alphaconfluence g

--All of this can also be done for let g = evaluate <some expression> <Some order of reduction>

