# Selecting the right data types

Choosing the right ADT for your application can have profound performance implications. By selecting the semantics that are most permissive or specialized for the particular use case, programmers can ensure that the system is given the best chance of scaling performance effectively. By allowing approximations or non-determinism, performance may be further improved.

## Probabilistic data types
Twitter's stream processing system \cite{summingbird} leverages a library of algebraic data types, known as Algebird, to allow easy composition of commutative operations. Among these data types are a number with probabilistic, rather than precise semantics.

## Conflict-free replicated data types

CRDTs, which were invented for eventual consistency, can actually be fit into our model as well, as a new kind of ADT with non-deterministic semantics. These data types might allow multiple copies to be kept in different shards of the database which asynchronously update each other on the operations they've seen. By defining the same kind of *merge* function as traditional CRDTs, these replicas can ensure that they all converge to the same state. For instance, a Set CRDT must decide what to do when two clients concurrently add and remove the same key — usually this is handled by choosing a preference for adds. Clients of this data type might find it more difficult to reason about, since a remove may appear to never have happened, but one can imagine making this tradeoff in parts of the application where it otherwise cannot scale. This mixing in of non-serializability should not be taken lightly. To ensure that serializable transactions aren't tainted by inconsistent information from these kinds of data types, some form of information flow or static analysis (via a type system) could be employed in client code.
