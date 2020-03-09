{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-|
Module      : Pure
Description : Pure functions that illustrate the basics of linear haskell.

We have simple linear functions and simple linear data structures that
illustrate the basic concepts of how the type checker of GHC with linear types
behaves.

-}


module Pure where




-- * Simple linear functions
------------------------------------------------------------

{-
   A linear function simply "consumes" it's argument exactly once.

   Creating a formal definition is quite tricky and requires a good
   amount of formalizing Haskell. A simple way to think about it is
   if (f :: a #-> b), then any case branch or leaf that (f x) could
   evaluate to, must either have the indivisible componenents
   that make up the first argument present exactly once
   in some data structure or by themselves, or, have each component
   be an arguement to exactly one linear function.

   Examples will make this clearer.

-}


linearIdentity :: a #-> a
linearIdentity x = x

{-
   The first argument must be used linearly since the arrow that follows it is
   linear. Here it is present once, without being in a data structure.
-}

linearSwap :: (a,a) #-> (a,a)
linearSwap (x,y) = (y,x)

{-

-}


data Holder where
  Holder :: a #-> Holder

linearHold :: a #-> Holder
linearHold x = Holder x

{-

Note that if Holder did not have the #-> then
this would not compile, because then you could use the value twice.
-}


data Holder2 where
  Holder2 :: a #-> b -> Holder2


linearHold' :: a #-> Holder2
linearHold' x = Holder2 x "hello"


linearApply :: (a #-> b) #-> a #-> b
linearApply f x = f x


{-

-}


-- Now, have some examples with the use of Unrestricted


-- * Using linear functions in non-linear functions
------------------------------------------------------------


regularIdentity :: a -> a
regularIdentity x = linearIdentity x


{-

   But this will fail

linIdentity :: a #-> a
linIdentity = regularIdentity x

-}






