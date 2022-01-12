{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Re where

data Re
  = Void
  | Lit Char
  | Alt Re Re
  | Cat Re Re
  | Star Re
  deriving (Show, Eq, Ord)

pattern Empty :: Re
pattern Empty = Star Void

-- a(b* + c)
ex1 :: Re
ex1 = Lit 'a' <> (Star (Lit 'b') `alt` Lit 'c')

-- Lit 'a' `Cat` (Star (Lit 'b') `Alt` Lit 'c')

alt :: Re -> Re -> Re
alt x Void = x
alt Void y = y
alt x y = Alt x y

instance Semigroup Re where
  Void <> _ = Void
  _ <> Void = Void
  x <> Empty = x
  Empty <> y = y
  x <> y = Cat x y

star :: Re -> Re
star = Star

deriv :: Char -> Re -> Re
deriv _ Void = Void
deriv c (Lit c') | c == c' = Empty
deriv _ (Lit _) = Void
deriv c (Alt x y) = deriv c x `alt` deriv c y
deriv c (Cat x y) =
  (deriv c x <> y)
    `alt` if nullable x then deriv c y else Void
deriv c (Star x) = deriv c x <> Star x

nullable :: Re -> Bool
nullable Void = False
nullable (Lit _) = False
nullable (Alt x y) = nullable x || nullable y
nullable (Cat x y) = nullable x && nullable y
nullable (Star _) = True

match :: String -> Re -> Bool
match s x = nullable (foldl (flip deriv) x s)
