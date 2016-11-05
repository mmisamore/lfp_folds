module Folds

/// Caveat: None of the following is original. I've picked it up here and there from years of reading
/// blog posts about functional programming. But I did write the code below.

// Suppose we want to add the elements of some list of numbers. We can do that pretty easily:
let sumList1 xs = List.sum xs : int 
// sumList1 [0 .. 100]

// Okay, what about the number of elements in this list?
let countList1 xs = List.length xs
// countList1 [0 .. 100]

// How do these work under the hood? (assuming we don't cache the answer somewhere)
let sumList2 xs = List.fold (+) 0 xs

// Another example:
let count2 xs = List.fold (fun s _ -> s + 1) 0 xs

// Here's the type of List.fold:
// List.fold : ('s -> 'x -> 's) -> 's -> 'x list -> 's

// Here, (+) : 's -> 'x -> 's  where 's is the sum accumulator. The 'x comes from the list xs being fed to us
// The initial value of 's is 0
// and the input list is "xs" which provides the 'x values to us

// This looks like a loop, but why write it down in this weird way? Bear with me...

// Fancier example of the same idea:
let mean1 xs = (List.fold (+) 0.0 xs) / (List.fold (fun s _ -> s + 1.0) 0.0 xs)
// mean1 [0.0 .. 100.0]
// ... but this sucks because we had to traverse the list xs twice! There must be a better way.

// What if we try to combine these two "summary" operations so we only have to traverse the list once?
let mean2 xs =
    let (totalSum,totalCount) = List.fold (fun (s,c) x -> (s+x,c+1.0)) (0.0,0.0) xs
    totalSum / totalCount
// mean2 [0.0 .. 100.0]

// This is probably the point at which most programmers think they have been awfully clever and walk away. After all,
// we've traversed the data once and provided an answer. What more do you want?

// Well, we had to mangle our two folds together to achieve this, violating Separation of Concerns  :-(
// I also used to think that the above solution was best possible, so let me tell you why I was wrong.

// First: what are we really trying to do here?
// Ans: we have some operations like (+) we are trying to perform at each step to combine our data, plus
//      we have initial starting points for our accumulators, plus
//      we want to do something at the end to combine the results

// How about this? Note that this already generalizes over the type of input (Lists, Sequences, etc.)
type Fold<'a,'s,'b> = private {
    step: ('s -> 'a -> 's);
    accum: 's;
    fetch: ('s -> 'b)
}

// Here's a smart constructor to build these things since the real constructor is private
let fold: ('s -> 'a -> 's) -> 's -> ('s -> 'b) -> Fold<'a,'s,'b> =
    fun step accum fetch -> { step = step; accum = accum; fetch = fetch }

// Here's the sum as a Fold
let sum = fold (+) 0.0 id

// and here's the count as a Fold
let count: Fold<float,float,float> = fold (fun s _ -> s + 1.0) 0.0 id

// Of course, we have to be able to use our folds with Lists:
let foldList: Fold<'a,'s,'b> -> 'a list -> 'b =
    fun fld xs -> fld.fetch (List.fold fld.step fld.accum xs)
// foldList sum [0.0 .. 100.0]
// foldList count [0.0 .. 100.0]

// Now time for some benefits of this approach. First, you can map over the outputs of any Fold:
let mapAfter: ('b -> 'c) -> Fold<'a,'s,'b> -> Fold<'a,'s,'c> =
    fun f fld -> fold fld.step fld.accum (fld.fetch >> f)
// let foldModified = mapAfter (fun x -> x + 42.0) sum 
// foldList foldModified [0.0 .. 100.0]

// This function lets us pretend that a Fold that consumes a's really consumes z's given
// any function of type 'z -> 'a
let mapBefore: ('z -> 'a) -> Fold<'a,'s,'b> -> Fold<'z,'s,'b> =
    fun f fld -> fold (fun s z -> fld.step s (f z)) fld.accum fld.fetch
// e.g. Square every input before summing:
// let squaredSum = mapBefore (fun x -> x * x) sum 
// foldList squaredSum [0.0 .. 10.0]

// Now for a trick that's more surprising. Any fold can be transformed into a resumable fold
let duplicate: Fold<'a,'s,'b> -> Fold<'a,'s,Fold<'a,'s,'b>> =
    fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)
// let resumableFold = foldList (duplicate sum) [0.0 .. 50.0]
// foldList resumableFold [51.0 .. 100.0]

// Extract the current value of any Fold from the accumulator
let extract: Fold<'a,'s,'b> -> 'b =
    fun fld -> fld.fetch fld.accum
// let resumableFold = foldList (duplicate sum) [0.0 .. 50.0]
// extract resumableFold

// A different way to resume Folds: we can make Fold<'a,'s,'x> resumable in the form Fold<'a,'s,Fold<'a,'s,'x>> and then
// mapAfter some function on the duplicated Fold result. This generalizes mapAfter!
let extend: (Fold<'a,'s,'x> -> 'y) -> Fold<'a,'s,'x> -> Fold<'a,'s,'y> =
    fun f fld -> mapAfter f (duplicate fld)

// Example: extend the "sum" fold by continuing to fold with more values after the initial values
// let extendExample = extend (fun fld -> foldList fld [51.0 .. 100.0]) sum
// foldList extendExample [0.0 .. 50.0]

// The main event: we can apply Folds to other Folds
let apFold: Fold<'a,'s,'x->'y> -> Fold<'a,'t,'x> -> Fold<'a,'s*'t,'y> =
    fun fxy fx -> fold
                    (fun (s,t) a -> (fxy.step s a, fx.step t a))
                    (fxy.accum, fx.accum)
                    (fun (s,t) -> (fxy.fetch s) (fx.fetch t))

// Introduce some notation
let (<!>) = mapAfter
let (<*>) = apFold

// Now we can compose our folds algebraically, getting single traversals for free
let betterMean = (/) <!> sum <*> count
// foldList betterMean [0.0 .. 100.0]

// ... and we can throw in extra values after the fact!
let resumedBetterMean = extend (fun fld -> foldList fld [51.0]) betterMean
// foldList resumedBetterMean [0.0 .. 100.0]

// More examples:
let sumSquaredDiffs m = mapBefore (fun x -> (x - m) ** 2.0) sum
let countMinusOne = mapAfter (fun x -> x-1.0) count
let variance xs = 
    let mean = foldList betterMean xs
    let var = (/) <!> sumSquaredDiffs mean <*> countMinusOne
    foldList var xs



// Appendix: Laws for Resumable Folds, with equational proofs

// Switching to resumable Fold and then extracting it back out again doesn't do anything (identity law 1)
// duplicate >> extract = (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)) >> extract
// = fun fld -> (fun s -> fold fld.step s fld.fetch) fld.accum
// = fun fld -> fold fld.step fld.accum fld.fetch
// = fun fld -> fld
// = id

// Switching to a resumable Fold and immediately extracting the value of the resumable part also doesn't do anything (identity law 2)
// duplicate >> mapAfter extract = (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)) >> (fun fld -> fold fld.step fld.accum (fld.fetch >> extract))
// = fun fld -> fold fld.step fld.accum ((fun s -> fold fld.step s fld.fetch) >> extract)
// = fun fld -> fold fld.step fld.accum (fun s -> fld.fetch s)
// = fun fld -> fold fold.step fld.accum fld.fetch
// = fun fld -> fld
// = id

// Switching to a resumable fold and then switching the return value into another resumable is the same as
// switching to a resumable and then turning that into a resumable (associativity)
//
// duplicate >> mapAfter duplicate
// = (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)) >> (fun fld -> fold fld.step fld.accum (fld.fetch >> duplicate))
// = fun fld -> fold fld.step fld.accum ((fun s -> fold fld.step s fld.fetch) >> duplicate)
// = fun fld -> fold fld.step fld.accum ((fun s -> fold fld.step s fld.fetch) >> (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)))
// = fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s (fun s -> fold fld.step s fld.fetch))

// duplicate >> duplicate
// = (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch)) >> (fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s fld.fetch))
// = fun fld -> fold fld.step fld.accum (fun s -> fold fld.step s (fun s -> fold fld.step s fld.fetch))
