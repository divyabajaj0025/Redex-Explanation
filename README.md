# Redex-Explanation

This project provides an explanation to the reduction sequence of a lambda expression. A lambda expression may have multiple redexes and any redex may be choosen in order to reduce that lambda expression. If no order is choosen it reduces as a graph with an individual expression as a node and all the expressions it can reduce to as its children. This reduction happens in the **Reduce module**.<br/>

The project also contains modules such as Normal and Applicative to define the reduction sequence for **Normal order** and **Applicative order**.<br/>
It is extensibility in the terms of defining a new order. A template to define a new reduction sequence order by a user is provided in the Order module.<br/>

#### Other modules:<br/>
* Expr - To define the syntax for Lambda expressions.
* Graph - To define the structure of Graphs. Nodes store expressions and Edges between 2 nodes store the relation between the two expressions represented by the nodes(Beta-reduction and Alpha-equivalence).
* Eval - To evaluate a single expression and output the next expressions it reduces to.
* Capture - To check for Variable capture and avoid it.
* Alpha - To check if two nodes in the graph are alpha confluent and connect them through unidirectional edges between them. This is more useful when we do not define a reduction sequence and produce a N-degree graph.
* Reduce - To produce a graph as output. It recurses on each eval sequence. Eval produces one step of the recursion that is a node and its children. Reduce recurses on it to create a complete sequence(graph in case of no reduction order).
* Views – To produce an evaluation graph and create explanation views on that Graph.
* NewViews - To add new views modularly without editing the existing ones.

To run this project. Commands:<br/>
ghci Views.hs<br/>
(This will load the rest of modules)<br/>
There are 4 expression examples in the **Test module** (e,e1,e2,e3)<br/>
To get some views on an expression:<br/>
>> output<br/>
Where a complete graph and a path to a node can be computed for the chosen expression.<br/>

### More views can be generated using the following commands:
###### To give out a complete graph:<br/>
>>view e<br/>
>>view e1<br/>
>>view e2<br/>
>>view e3<br/>

###### To give out sequence through Normal order :<br/>
>>evaluate' e normalRedex<br/>
>>evaluate' e1 normalRedex<br/>
>>evaluate' e2 normalRedex<br/>
>>evaluate' e3 normalRedex<br/>

###### To give out sequence through Applicative order :<br/>
>>evaluate' e applicativeRedex<br/>
>>evaluate' e1 applicativeRedex<br/>
>>evaluate' e2 applicativeRedex<br/>
>>evaluate' e3 applicativeRedex<br/>

##### For views on output<br/>
###### To find a context, i.e., what are the children of a node<br/>
>>let g = views e3<br/>
>>context 5 Down g<br/>
--5 is the index to a node<br/>

###### To find the nodes that reduces to a particular node<br/>
>>let g = views e3<br/>
>>context 5 Up g<br/>

###### To check for alphaconfluent nodes<br/>
>>let g = views e3<br/>
>>alphaconfluence g<br/>

*All of this can also be done for let g = evaluate (some expression) (Some order of reduction)*

