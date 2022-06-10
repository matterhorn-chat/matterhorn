module Matterhorn.Types.NonemptyStack
  ( NonemptyStack
  , newStack
  , push
  , pop
  , top
  , stackToList
  )
where

import Prelude ()
import Matterhorn.Prelude

-- | A stack that always has at least one value on it.
data NonemptyStack a =
    NonemptyStack { bottom :: !a
                  -- ^ The value at the bottom that can never be
                  -- removed.
                  , rest :: ![a]
                  -- ^ The rest of the stack, topmost element first.
                  }
                  deriving (Show)

-- | Make a new stack with the specified value at the bottom.
newStack :: a -> NonemptyStack a
newStack v = NonemptyStack { bottom = v
                           , rest = []
                           }


-- | Return the stack as a list, topmost element first.
stackToList :: NonemptyStack a -> [a]
stackToList (NonemptyStack b as) = as <> [b]

-- | Pop the top value from the stack. If a value could be popped,
-- return the new stack and the value that was popped. Otherwise return
-- the stack unmodified with @Nothing@ to indicate that nothing was
-- popped.
pop :: NonemptyStack a -> (NonemptyStack a, Maybe a)
pop s@(NonemptyStack _ []) = (s, Nothing)
pop s@(NonemptyStack _ (a:as)) = (s { rest = as }, Just a)

-- | Push the specified value on to the stack.
push :: a -> NonemptyStack a -> NonemptyStack a
push v s = s { rest = v : rest s }

-- | Return the value on the top of the stack. Always succeeds since the
-- stack is guaranteed to be non-empty.
top :: NonemptyStack a -> a
top (NonemptyStack _ (a:_)) = a
top (NonemptyStack bot []) = bot
