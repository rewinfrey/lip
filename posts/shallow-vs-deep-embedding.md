# Shallow vs. Deep Embedding

2018-09-18

The `NanoParsec` module described in Stephen Diehl's [tutorial](http://dev.stephendiehl.com/fun/002_parsers.html)
outlines data types and operations for parsing string data using a technique called
parser combinators. The core data type is defined as `Parser a` in which the `a`
type variable represents the type of a given parser's return value.
For example, `Parser Char` represents a parser that returns a single character.
The operations defined in `NanoParsec` can be grouped into two basic categories.
The first category of operations are primitive operations based on methods from
the Haskell type classes of `Functor`, `Alternative`, `Applicative`, `Monad`,
and `MonadPlus`. Understanding these methods is out of scope for this post. The
second category of operations are combinators that act similarly to smart constructor
helper functions and additionally may compose other combinator operations.
For example, the `char` combinator is a smart constructor for `Parser a` because
its return value's type is `Parser Char`. The `char` combinator is also defined
in terms of the `satisfy` combinator whose operation is defined in part by `bind`
or `(>>=)` from the `Monad` type class. Both the `Parser a` data type and the
combinators exported by the `NanoParsec` module form a domain specific language (DSL).
Although there is much to learn and discover about parser combinators, and while
the `NanoParsec` module is an excellent introduction to this technique, this post
is instead interested in exploring the differences between deep and shallow
embeddings of DSLs.

First let's begin with a look at shallow embeddings. A shallow embedding defines an
operation and that operation's semantics together as one unit. Most DSLs are shallow
embeddings, and the `NanoParsec` DSL is an example of a shallow embedding. To better
understand what is meant by "a shallow embedding defines an operation and that
operation's semantics together", let's look at an example taken from `NanoParsec`:

```haskell
newtype Parser a = Parser { parse :: String -> [(a, String)] }

...

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

...
```

The `bind` operation defined here as a function in the `NanoParsec` DSL also defines
its semantics directly within its function body. This is called a shallow embedding
because there is one and only one way to interpret what the `bind` operation within
the `NanoParsec` DSL means. If you are new to the idea of shallow embeddings you
may be wondering, "Why is this distinction important?", and "Don't all DSLs work
this way?". Shallow embeddings are the most ubiquitous form of DSLs we consume or
write, but it turns out this distinction is important if we wish to provide a
degree of freedom between the definition of an operation and its semantics.

Notice the `bind` operation above is defined in terms of `concatMap`. This tells
us that working within the `NanoParsec` DSL implicitly means working with lists.
However, what if it was more desirable for us to use `Maybe` instead? How would
we proceed to change `NanoParsec` to return `Maybe` results instead of lists?
Because `NanoParsec` is a shallow embedding in which every operation's
definition also defines its semantics, changing its operations to support `Maybe`
requires us to change the definition for every operation. But what about other
consumers that depend on the list implementation? Thus we have a dilemma. To
change the semantics of a given DSL's operations necessarily requires us to change
every operation and adversely affects other consumers of that DSL.

You may be thinking, "What if we define the operations to support both lists and
`Maybe` types? We can then encode additional data in the `Parser a` type such that
the operations can switch or case on that new data to return the desired result type."
This certainly can work but it is not ideal. What happens when a consumer requests
a new result type other than list or `Maybe`? For each such request we are required
to update our DSL's core operations. This is the functional programming equivalent
of a violation of the [open-closed principle](https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle)
descrbed in object-oriented programming. Our abstractions, in this context
our DSLs, should be open for extension such as supporting a new return
type, but closed for modification. Not to mention code churn in critical paths
is always risky.

There's also a troubling aspect of coupling `Parser a` to the implementation
of our operations when we encode additional data in `Parser a` that is only used to
inform the runtime semantics of our operations. I would argue this is a violation
of the [dependency inversion principle](https://en.wikipedia.org/wiki/Dependency_inversion_principle). Both data
and operations are a form of abstraction and thus should not depend on details like
encoded data meant to inform the runtime semantics for some operation to specify a
result type. Instead, as the dependency inversion principle states, details should
depend on abstractions. In this case, the detail is the result type desired from a DSL,
but what is a suitable abstraction on which this detail can depend? That abstraction
should allow us to choose the result type we want without modifying our operations,
and it should allow us to extend our DSL to support new result types in the future.
A deep embedding is such an abstraction.

Deep embeddings allows us to specify multiple semantic versions for an operation
(e.g. `bind`) without modifying an operation's definition. This is possible because
deep embeddings require a separation between the definition of an operation and
the specification of that operation's semantics. What does the definition of an
operation mean if we don't also include a specification for its semantics? By defining
an operation only in terms of a data representation provides us with the freedom
to interpret that data representation for an operation with whatever semantics are
desired. In other words, the interpreter we apply to operations (now represented
only as data) specifies the semantics under which an operation is evaluated, and
ultimately, what result type is returned.

Let's take a look at an example of a deep embedding for `NanoParsec`'s `bind`
operation.

```haskell
data Parser a where
  ...
  Bind :: Parser a -> (a -> Parser b) -> Parser b
  ...

bind :: Parser a -> (a -> Parser b) -> Parser b
bind = Bind

listInterpreter :: Parser a -> String -> [(a, String)]
listInterpreter p s = case p of
  ...
  Bind p f -> concatMap (\(a, s') -> eval (f a) s') (eval p s)
  ...
```

Like the shallow embedding example seen previously, the `bind` operation still
returns a `Parser a` type but no longer directly specifies its semantics. Instead,
this deep embedding represents `bind` as a unique data constructor `Bind`, that is
interpreted using the `listInterpreter` function. The `listInterpreter` specifies
the semantics for `Bind` in the same manner as before in the shallow embedding,
but there is a clean separation between where the `bind` operation is defined and
the semantics used when evaluating a `bind` operation.

By virtue of using a deep embedding, not only is it possible to define a `listInterpreter`,
but we can also additionally define a `maybeInterpreter` if we desire a `Maybe`
result type from our `NanoParsec` operations. The deep embedding approach ensures
that the open-closed principle is never violated. By defining the `NanoParsec` DSL
as a deep embedding, we can extend it to accommodate other return types in the
future via new interpreters without modifying its existing operations.

At this point you may be thinking, "Wow, deep embeddings are the new hotness,
why aren't all DSLs defined as a deep embedding?". The answer is variable, but
it is important to understand that deep embeddings are not a zero cost abstraction.
For example, if we wanted to add a new operation to our `NanoParsec` deep embedding DSL,
it would require us to also modify each existing interpreter to account for that
new operation. This makes the cost of adding or removing operations from a deep embedding
more costly compared with a shallow embedding.

Although it is easier to add or remove an operation from a shallow embedding,
it is far more complex to change the semantics of an operation in a shallow embedding
compared with a deep embedding. Depending on the scope of the change to an operation's
semantics in a shallow embedding, changes to semantics of one operation may require
changes to the semantics of all other operations. However, in a deep embedding,
changing the semantics of an operation is often as simple as adding a new interpreter.

Below are some general guidelines that may be useful for deciding whether a deep or shallow embedding is suitable for your needs:

* Do you need the flexibility of multiple result types for your DSL? If yes, a deep embedding will reduce churn and complexity.
* Will you need to continuously update your DSL with new operations? If yes, a shallow embedding may be faster and easier to manage, but if multiple result types are required a deep embedding is worth the added churn.
* Does your DSL define numerous operations? If yes and depending on answers to the first two questions, it may be advantageous to start with a shallow embedding and move to a deep embedding when multiple result types become required (if ever).

### Thanks

This post and learning would not have been possible without [Patrick Thomson](https://github.com/patrickt).
Patrick first introduced me to the idea of deep embeddings and graciously offered
his time and help in implementing the deep embedding version of `NanoParsec`.
Thank you for your help, Patrick!

### Code

* [`NanoParsec` shallow embedding](https://github.com/rewinfrey/lip/blob/master/src/Data/Parser/ShallowNanoParsec.hs)
* [`NanoParsec` deep embedding](https://github.com/rewinfrey/lip/blob/master/src/Data/Parser/DeepNanoParsec.hs)

### References

Gibbons, Jeremy. "Folding Domain-Specific Languages: Deep and Shallow Embeddings". Referenced from https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/embedding-short.pdf.

Vermeulen, Alessandro. "The difference between shallow and deep embedding". Referenced from https://alessandrovermeulen.me/2013/07/13/the-difference-between-shallow-and-deep-embedding/
