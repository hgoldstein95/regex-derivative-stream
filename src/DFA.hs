{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module DFA where

import Re (Re (..))
import Prelude hiding (fail)

data DFA = DFA
  { accepting :: Bool,
    next :: Char -> DFA
  }

-- accepting :: DFA -> Bool
-- next :: DFA -> Char -> DFA

ex1 :: DFA
ex1 = start
  where
    start = DFA {accepting = False, next = \c -> if c == 'a' then x else fail}
    x = DFA {accepting = False, next = \c -> if c == 'c' then y else if c == 'b' then z else fail}
    y = DFA {accepting = True, next = const fail}
    z = DFA {accepting = True, next = \c -> if c == 'b' then z else fail}
    fail = DFA {accepting = False, next = const fail}

runDFA :: String -> DFA -> Bool
runDFA [] a = accepting a
runDFA (x : xs) a = runDFA xs (next a x)

convert :: Re -> DFA
convert Void = void
convert (Lit c) = lit c
convert (Alt x y) = alt (convert x) (convert y)
convert (Cat x y) = cat (convert x) (convert y)
convert (Star x) = star (convert x)

void :: DFA
void = DFA {accepting = False, next = const void}

lit :: Char -> DFA
lit c =
  DFA
    { accepting = False,
      next = \c' -> if c == c' then star void else void
    }

alt :: DFA -> DFA -> DFA
alt x y =
  DFA
    { accepting = accepting x || accepting y,
      next = \c -> next x c `alt` next y c
    }

cat :: DFA -> DFA -> DFA
cat x y =
  DFA
    { accepting = accepting x && accepting y,
      next = \c ->
        (next x c `cat` y)
          `alt` if accepting x then next y c else void
    }

star :: DFA -> DFA
star x =
  DFA
    { accepting = True,
      next = \c -> next x c `cat` star x
    }
