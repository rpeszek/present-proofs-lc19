# present-proofs-lc19

Code accompanying LC2019 talk "Write You Some Proofs for Great Good"

Presentation Description
------------------------

What do mathematical proofs have to do with programming? There is the concept of verified code or the Curry-Howard isomorphism. Anything more down to earth? This talk will be about the need for writing proofs to persuade the compiler to compile your code.

Haskell is not dependently typed but with extensions like `GADTs` or `DataKinds`, we can implement some precise types. Think `Vect (n :: Nat) a` (fixed size list) or something much simpler like `Maybe` parameterized by a `Bool` type variable so you know if it is `Just` or `Nothing` at the type level. This increased type safety comes with a cost and causes the type checker to sometimes throw up its hands. The programmer can help out by providing proofs of type equality.

This yields an interesting tradeoff: a programming nirvana where programs are very type safe but we need to pay for that safety with extra code. Such programming can feel brittle and uses concepts like propositional proofs that are not part of the software engineer toolbox.  We will discuss dependent types and proofs while wearing software engineering hats. We will talk about: design, maintenance cost, explain brittleness, look at performance, and other software engineering things.

We will also try programming some proofs, programming contradictions, design types that are not just gentlemen's agreements but represent true logical propositions.

I will use Haskell with a touch of Idris. I will try to keep things intuitive (assuming no prior knowledge of dependent types or Idris). Some familiarity with `GADTs` and `DataKinds` would be helpful but not essential.




