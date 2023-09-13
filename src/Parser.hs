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
{- WORKING
instance Monad Parser where 
  return = pure
  (P x) >>= f = P (\s ->  
                          let os = do 
                                    (s', P fb) <- [(s, f a') |  (s', a')    <-   x s ]
                                       
                                    return $ fb s' 
                          in concat os     -- could use join here instead of concat  
-}

instance Monad Parser where 
  return = pure
  (P x) >>= f = P (\s -> do  
                         (s', P fb) <- [(s, f a') |  (s', a')    <-   x s ]
                         (s', b)    <- fb s'
                         return (s', b) 
                      



--    oncat ( (\(s', a') -> (s', (f a'))) <$> (x s)) 
--                              (s'', bs) = (\(s', (P fb)) -> (s', fb s')) <$> os    
--                          in  (s'', bs)
                  )
  
  
 
--convert :: Functor f => [(String, f a) ] -> f [(String, b)]                       
--convert list = let mbs = [(s', f q') | (s', q') <- x s ]    
--               in  \s -> (traverse (\(x, P y) -> P (x,y)) mbs)   
                                 





