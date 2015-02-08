# Data type selection

Choosing the right data type for your application can have profound performance implications.
As mentioned before, the interface can determine what commutes simply by exposing more information than necessary. By selecting the semantics that are most permissive or specialized for their particular use case, programmers can ensure that the system is given the best chance of scaling performance effectively. By allowing approximations or non-determinism, performance may be further improved.

## Unique identifiers
As an extremely simple motivating example, imagine a programmer wants a unique identifier for each new post. They might naively choose a `Counter` which returns the next number in the sequence whenever `next` is called. Ensuring each caller gets the next value without skipping any ends up being very expensive, as implementers of TPC-C \cite{TPCC}, which explicitly requires this, know well. In most cases, however, the programmer could use a special `UniqueID` type, whose only criteria is that each call to `next` returns a different value, which can be implemented trivially in a number of efficient highly-concurrent ways.

## Probabilistic data types

Some data types have *probabilistic* guarantees about their semantics, which, rather than always returning a precisely correct answer, trade off some accuracy for better performance or storage. Some better-known examples include *bloom filters* \cite{bloom}, *hyperloglog* \cite{hyperloglog}, and *count-min sketch* \cite{countminsketch}. Hyperloglog, which also appears in Redis \cite{redis}, estimates the cardinality (size) of a set within a fixed error bound. Twitter's streaming analytics system \cite{summingbird} leverages these probabilistic data types to handle the high volume of data needing to be processed. We expect similar improvements to be had from their use in Claret.

## Conflict-free replicated data types

CRDTs, which were invented for eventual consistency, can actually be fit into our model as well, as a new kind of data type with loosly synchronized semantics. These data types might allow multiple copies in different shards to asynchronously update each other on the operations they've seen. By defining the same kind of *merge* function as traditional CRDTs, these replicas could ensure they all converge to the same state. For instance, a Set CRDT must decide what to do when two clients concurrently add and remove the same key — usually this is handled by choosing a preference for adds. Clients of this data type might find it more difficult to reason about, since a remove may appear to never have happened, but one can imagine making this tradeoff in parts of the application where it otherwise cannot scale.

This mixing in of non-serializability should not be taken lightly. To ensure that serializable transactions aren't tainted by inconsistent information from these kinds of data types, some form of information flow or static analysis (via a type system) could be employed in client code.
