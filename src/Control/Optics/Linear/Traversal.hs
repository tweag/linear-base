-- | This module provides linear traversals.
--
--  Traversals provides a means of accessing several @a@s organized in some
--  structural way in an @s@, and a means of changing them to @b@s to create a
--  @t@. In very ordinary language, it's like walking or traversing the data
--  structure, going across cases and inside definitions. In more imaginative
--  language, it's like selecting some specific @a@s by looking at each
--  constructor of a data definition and recursing on each non-basic type
--  (where basic types are things like @Int@, @Bool@ or @Char@).
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE GADTs #-}
--
-- import Control.Optics.Linear.Internal
-- import qualified Control.Functor.Linear as Control
-- import Control.Functor.Linear ((<$>), (<*>), pure)
-- import Prelude.Linear
--
-- -- We can use a traversal to append a string only to the
-- -- human names in a classroom struct
-- appendToNames :: String -> Classroom %1-> Classroom
-- appendToNames s = over classroomNamesTrav (\name -> name ++ s)
--
-- data Classroom where
--   Classroom ::
--     { className :: String
--     , teacherName :: String
--     , classNum :: Int
--     , students :: [Student]
--     , textbooks :: [String]
--     } %1-> Classroom
--
-- -- A Student is a name and a student id number
-- data Student = Student String Int
--
-- classroomNamesTrav :: Traversal' Classroom String
-- classroomNamesTrav = traversal traverseClassStr where
--   traverseClassStr :: forall f. Control.Applicative f =>
--     (String %1-> f String) -> Classroom %1-> f Classroom
--   traverseClassStr onName (Classroom cname teachname x students texts) =
--     Classroom <$>
--     pure cname <*>
--     onName teachname <*>
--     pure x <*>
--     traverse' (\(Student s i) -> Student <$> onName s <*> pure i) students <*>
--     pure texts
-- @
--
module Control.Optics.Linear.Traversal
  ( -- * Types
    Traversal, Traversal'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , traversed
    -- * Using optics
  , over, overU
  , traverseOf, traverseOfU
    -- * Constructing optics
  , traversal
  )
  where

import Control.Optics.Linear.Internal
