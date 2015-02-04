# Commutativity
\label{sec:commutativity}

- *discuss formal representations of commutativity from \cite{Herlihy:PPoPP08} and \cite{Kulkarni:PLDI11}, tradeoff between maximum concurrency and cost (runtime and implementation) of tracking it* 


Commutativity is well known, especially in distributed systems, for enabling important optimizations. Beginning in classic databases literature from the 70s and 80s, commutativity was exploited by database systems designers within the safe confines of relational models. They were able to use their deep, complete knowledge of the data model and query plans to determine which transactions conflict with one another. *(citations?)*

Since then, commutativity has seen a resurgence in modern NoSQL systems which have forgone complex data models for increased flexibility and scalability. In systems with eventual consistency, Conflict-free Replicated Data Types (CRDTs)\cite{Shapiro:SSS11} are defined in such a way that all their operations commute with each other to ensure that all replicas converge to the same state, regardless of the order in which operations are executed. In CRDTs, this is done by defining *merge* semantics which, while simpler than unbounded eventual consistency, may still be difficult for programmers to reason about. For instance, a Set CRDT must decide what to do when two clients concurrently add and remove the same key — usually this is handled by choosing a preference for adds. The result is that the client who removed the key must understand that their operation may not have happened.

## Commutativity Specifications


## Transaction Boosting


## Other opportunities
