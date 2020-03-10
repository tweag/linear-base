{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}

{-|
Module      : Pure
Description : Pure functions showing the basics of linear haskell.

We have simple linear functions and simple linear data structures that
illustrate the basic concepts of how the type checker of GHC with linear
types behaves.
-}


module Pure where

import Prelude


-- * Simple linear functions
------------------------------------------------------------

{-
   A linear function simply "consumes/uses" it's argument exactly once.

   Creating a formal definition is quite tricky and requires a good amount
   of formalizing Haskell. A simple way to think about it is if (f :: a
   #-> b), then as (f x) evaluates, x and/or the linear components of x,
   are syntactically present exactly once, and if in a data structure, is
   a linear argument to a constructor.

   Examples will make this clearer.
-}


linearIdentity :: a #-> a
linearIdentity x = x

{-
   We say the first argument is linear since the arrow that follows it is
   linear. In the RHS, it is present once, without being in a data
   structure.
-}


linearSwap :: (a,a) #-> (a,a)
linearSwap (x,y) = (y,x)

{-
   Here, the argument is decomposed by the tuple data constructor into two
   pieces. Since the whole first argument is linear, the tuple constructor
   in linear haskell is linear -- i.e., notice the type of the
   constructor:

   (,) :: a #-> b #-> (a,b)

   Now, this does not mean that if we have some non-linear function with
   an argument (a,b) that `a` and `b` have to be consumed exactly once.

   A linear arrow in a data constructor merely signifies that the argument
   that preceedes that arrow must be used linearly if the input to a
   linear function is this data type with this constructor. Here, since
   `(,) x y` was the first input to linearSwap, which is a linear function
   (meaning the first input is linear), the components `x` and `y` must
   be used linearly.  With a non-linear function, this is not the case.

   Consider the next function as an example.
-}

nonLinearSubsume :: (a,a) -> (a,a)
nonLinearSubsume (x,_) = (x,x)

{-
   Notice that the function above could not have a linear arrow. If it
   did, it would not compile. Why is this?  Well, the first argument is
   linear, and the constructor is linear in both components.

   (,) :: a #-> b #-> (a,b)

   Again, this means the `a` and `b` must be used linearly.

   Yet, in the body of the function, `a` is used twice and `b` is used
   zero times.
-}


linearIdentity2 :: a #-> a
linearIdentity2 x = linearIdentity x

{-
   To use an input linearly (or a component of an input), we can pass it
   to another linear function. Here, since linearIdentity is linear, we
   can be sure the term (linearIdentity x) consumes x exactly once.
   Hence, all of linearIdentity2 consumes the input x exactly once.

   If we replaced it with the original `id`, this would fail to type check
   because `id` has the non-linear type `a -> a`.  Thus, GHC isn't sure
   that `id` uses its input exactly once.  If `id` doesn't use it's input
   exactly once, then linearIdentity2 won't use it's input exactly once,
   violating it's type signature.

   Now, this does not mean that merely using a linear function makes the
   use of a variable linear. For instance, both of the two functions below
   use their input exactly twice.
-}

nonLinearPair :: a -> (a,a)
nonLinearPair x = (linearIdentity x, linearIdentity x)

nonLinearPair2 :: a -> (a,a)
nonLinearPair2 x = (x, linearIdentity x)

{-
   Hopefully, this gives a basic sense of the arithmatic of how much an
   input to a function is "used".
-}


regularIdentity :: a -> a
regularIdentity x = linearIdentity x

{-
   Of course, the fact that a function is linear makes no difference in
   non-linear functions. So, non-linear functions can call linear
   functions willy-nilly and they will work as expected. To state the
   obvious, linear functions are regular functions but not all functions
   are linear functions.
-}


(#.) :: (b #-> c) -> (a #-> b) -> (a #-> c)
g #. f = \a -> g (f a)

linearCompose :: (a,a) #-> (a,a)
linearCompose = linearIdentity #. linearSwap

{-
   Above, we compose two linear functions and write a linear version of
   `(.)`.  Here, as before, it is critical that we are composing linear
   functions.  Notice that we cannot write a function of the following
   type:

   (##.) :: (b -> c) -> (a #-> b) -> (a #-> c)
-}



-- * Linear functions with user data types
------------------------------------------------------------

{-
  We can consume linearly bound inputs into data types if the constructor
  has a linear arrow before the input.
-}

data LinearHolder a where
  LinearHolder :: a #-> LinearHolder a

linearHold :: a #-> LinearHolder a
linearHold x = LinearHolder x

{-
   Note that if the constructor LinearHolder did not have the #-> then
   linearHold would not compile, because then you could use the value
   non-linearly.
-}


linearHoldExtract :: LinearHolder a #-> a
linearHoldExtract (LinearHolder x) = x

linearIdentity3 :: a #-> a
linearIdentity3 = linearHoldExtract #. linearHold

{-
   For clarity, we include an example of using such linear constructors.
   Here, linearHoldExtract must use the inner component linearly.
   Therefore, it's impossible to implement the following function:

   linearHoldPair :: LinearHolder a #-> (a,a)
   linearHoldPair (LinearHolder x) = (x,x)

   In fact, we have the following equivalency:

   (LinearHolder a  #-> b) ≅ (a #-> b)
-}


data LinearHolder2 where
  LinearHolder2 :: a #-> b -> LinearHolder2

linearHold' :: a #-> LinearHolder2
linearHold' x = LinearHolder2 x "hello"
--linearHold' x = LinearHolder2 "hi" x -- fails to type check

{-
   We can have constructors with mixed arrows, of course.  Here, this
   means only the first value is bound linearly.  This is why the
   commented out line would fail to type check
-}


data ForcedUnlinear a where
  ForcedUnlinear :: a -> ForcedUnlinear a

forcedLinearPair :: ForcedUnlinear a #-> (a,a)
forcedLinearPair (ForcedUnlinear x) = (x,x)

{-
   Above we define a data type ForcedUnlinear which does not use the
   linear arrow to hold it's argument. This means that even if an input of
   type `ForcedUnlinear a` is linear, the component does not have to be.
   Hence, we can write the function above but could not write something
   the following type:

   linearPair :: a #-> (a,a)
-}


demote :: (ForcedUnlinear a #-> b) -> (a -> b)
demote f x = f (ForcedUnlinear x)

promote :: (a -> b) -> (ForcedUnlinear a #-> b)
promote f (ForcedUnlinear x) = f x

{-
   Another way of saying this is the following equivalence proven by the
   two functions above:

   (ForcedUnlinear a  #-> b) ≅ (a -> b)

   In the linear haskell POPL '18 paper, this datatype is called
   UnRestricted.  (It's renamed here for clarity within this context.)
-}
