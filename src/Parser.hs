{-# LANGUAGE ScopedTypeVariables  #-}


module Parser 
  (
    Parser(..)
  ,  parse 

  ) where


import Control.Applicative 

import Control.Monad (join)

newtype Parser a = P (String -> [(String, a)])

parse :: Parser a -> String -> [(String, a)]
parse (P p) s  = p s

instance Functor Parser where 
  fmap f (P fx) =   P   (\s ->  (map (\(s, x) -> (s, f x)) (fx s) ) )

instance Applicative Parser where 
  pure x = P (\s -> [(s, x)])
  (<*>) (P f) (P px) =  P  (\s   ->  
                               do
                                (s', x')      <- px s
                                (s'', aToB )  <- f s'
                                return (s'', aToB x')  )


instance Monad Parser where 
  return = pure
  (P x) >>= f = P (\s -> do  
                         (s', P fb) <- [(s, f a') |  (s', a')    <-   x s ]
                         (s', b)    <- fb s'
                         return (s', b) 
                  )




