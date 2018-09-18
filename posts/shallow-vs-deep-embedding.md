# Shallow vs. Deep Embedding

The `NanoParsec` parser described in Stephen Dieh's [tutorial](http://dev.stephendiehl.com/fun/002_parsers.html)
provides data and operations common to parsing in a functional context. The data is in the form of a `Parser` type
parameterized by a type variable representing the parser's return value. `Parser Char` therefore represents a parser that returns
characters. `Parser String` represents a parser that return strings, etc. The parser operations in `NanoParsec` are based on methods
from common Haskell type classes such as `Monad`, `Applicative`, `Functor`, `Alternative`, and `MonadPlus`. Lastly, combinators are
created for convenience that construct data (i.e. parsers) in conjunction with operations that yield results in relation to other
combinators and their parser / operation semantics. All of this is to say that `NanoParsec` defines a domain specific language for
describing the data and operations of parsing. Two terms associated with domain specific languages (DSLs) are shallow and deep
embeddings.

The approach outlined in Stephen Diehl's tutorial is that of a shallow embedding. Namely, that the semantics of parser operations are
defined together with the definition of a parser operation:

```haskell
newtype Parser a = Parser { parse :: String -> [(a, String)] }

...

failure :: Parser a
failure = Parser (\cs -> [])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

...
```

This snippet of `NanoParsec`  enough to illustrate the concept of a shallow embedding. Namely, we have data (`Parser a`)
and operations (`failure` and `bind`) whose definitions also define their semantics. For example, `bind` defines what the bind
operation on `Parser a` data means. This is the essence of a shallow embedding: when defining an operation, one is also required
to define the semantics for that operation. An operations in a shallow embedding therefore can only have one definition for its semantics.
However, what if we want to have flexibility to define the `bind` operation over our `Parser a` data in various ways depending on the context or application?

A shallow embedding will not allow us to define multiple semantic definitions for `bind` without adding new bind operations whose names differ, or by encoding
additional parameters into our data that `bind` can pattern match against. While one _could_ do this it is not generally thought of as a good idea. We apply
operations to data. The moment we change our data to know something about our operations we set ourselves up for trouble. If our operations change, that also means
potentially changing our data. The semantics of our operations should be flexible to change without requiring changes to our data.

Deep embeddings allows us to define multiple semantic definitions for an operation (e.g. `bind`) without adding new operations. This is possible because deep embeddings
separate the definition of an operation from the definition of that operation's semantics. Because we can separate an operation's definition from its semantics, operations
in a deep embedding are more akin to data construction that is interpreted or evaluated by one or more interpreters. The choice of interpreter for evaluating an operation
provides the choice of semantic definition used when interpreting that operation.

```haskell
data Parser a where
  ...
  Bind    :: Parser a -> (a -> Parser b) -> Parser b
  ...

bind :: Parser a -> (a -> Parser b) -> Parser b
bind = Bind

listEval :: Parser a -> String -> [(a, String)]
listEval p s = case p of
  ...
  Bind p f     -> concatMap (\(a, s') -> eval (f a) s') (eval p s)
  ...
```

This snippet of a deep embedding for `NanoParsec` focuses only on one operation, `bind`. Here it is possible to see that the definition for the `bind` operation
is a data constructor for `Bind`. The semantics for how the `bind` operation is interpreted are defined in `listEval`. This interpreter uses the `List` monad's version of
bind to generate a list of parser results. If we wanted to provide a different return type for our parser operations, for example `Maybe`, we could also define a `maybeEval`
interpreter. A deep embedding, because it separates the definition of an operation from that operation's semantics, allows operations to be defined using multiple semantic definitions in the form of an interpreter.

However, deep embeddings are not a zero cost abstraction. Adding a new operation to a deep embedding is more expensive compared with a shallow embedding. If we add a
new operation to our deep embedding, we must update the data representation (in this instance the `Parser a` GADT) and each interpreter for our data. For instance,
if we previously defined three interpreters for `Parser a`, then adding a new operation to `Parser a` requires us to update those three interpreters to include the
new operation. Even though deep embeddings give us a degree of freedom between an operation's definition and its semantics, adding operations to a deep embedding is more complex and costly compared with a shallow embedding.

Because a shallow embedding couples the definition of an operation and its semantics, adding new operations to a shallow embedding is straightforward and less costly. This
ease of adding operations in a shallow embedding is offset by the immense difficulty and complexity of changing the semantics of a single operation. If we want `Parser a` to
return a `Maybe a` instead of a `List a` that means updating every operation's semantic definition to accommodate the new data shape. This one to one mapping of an operation and
its semantics also prevents us from being able to interpret operations over some given data in multiple ways. Despite the ease at which we can add new operations to a shallow
embedding, the complexity of changing the semantics of operations or evaluating to a different data type is much more complex and costly compared with deep embeddings.

General guidelines may be useful for deciding whether a deep or shallow embedding is more suitable for your needs:

* Do you need the flexibility of multiple return types for your DSL's use case? If so, a deep embedding can reduce complexity.
* Do you have many operations to encode in the DSL? If so, a shallow embedding may be easier to maintain over time.
* Can you depend on type class abstractions for your operations? If so, a deep embedding is pleasantly well suited to reusing type class abstractions.

For a majority of use cases a deep embedding will provide the maximum flexibility at the expense of slightly more code as the number of interpreters increases.

### References

Gibbons, Jeremy. "Folding Domain-Specific Languages: Deep and Shallow Embeddings". Referenced from https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/embedding-short.pdf.

Vermeulen, Alessandro. "The difference between shallow and deep embedding". Referenced from https://alessandrovermeulen.me/2013/07/13/the-difference-between-shallow-and-deep-embedding/
