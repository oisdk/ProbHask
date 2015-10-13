import Control.Applicative
import Data.Ratio
import Data.List
import Data.Ord
import Data.Maybe
import System.Random
import qualified Data.Map.Strict as M
import Data.Tuple
import Data.Either
import qualified Data.Foldable as F

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  

instance Functor Prob where  
  fmap f (Prob xs) = Prob [(f x, p) | (x,p) <- xs]
    
instance Applicative Prob where
  pure a = Prob [(a,1)]
  (Prob ys) <*> (Prob xs) = Prob [(f x, px*py)|(f,py) <- ys, 
                                               (x,px) <- xs]
  
instance Monad Prob where
  return = pure
  (Prob xs) >>= f = Prob [(y,px*py)|(x,px) <- xs, 
                                    (y,py) <- getProb(f x)]
  
equalProbs :: [a] -> Prob a
equalProbs x = Prob $ (flip (,) n) <$> x
  where n = 1 % fromIntegral (length x)
     
sumSnd :: Num b => (a,b) -> (c,b) -> (c,b)
sumSnd = fmap . (+) . snd

mergeProbs :: Ord a => Prob a -> Prob a
mergeProbs = Prob . mergeBy sumSnd (comparing fst) . getProb

eqOf :: (a -> a -> Ordering) -> (a -> a -> Bool)
eqOf c = (\a b -> case c a b of EQ -> True 
                                _  -> False)

mergeBy :: (a -> a -> a) -> (a -> a -> Ordering) -> [a] -> [a]
mergeBy m c = (foldl1' m <$>)  . 
              groupBy (eqOf c) . 
              sortBy c

randRatio :: Integer -> IO Rational
randRatio d = (%d) <$> getStdRandom (randomR (1,d))

choose :: Prob a -> IO a
choose = (<$> randRatio 1000) .
         flip upTo            . 
         scanl1 sumSnd        . 
         getProb
   where upTo n = fst         . 
                  fromJust    . 
                  find ((> n) . snd)
                                
expected :: Integral a => Prob a -> Rational
expected =  sum . (uncurry (*) <$>) . getProb . (fromIntegral <$>) 

maxBy :: (a -> a -> Ordering) -> [a] -> a
maxBy c = foldr1 comp
  where comp a b = case c a b of GT -> a
                                 _  -> b
                                 

likliest :: Prob a -> a
likliest = compRest 1 . getProb
  where compRest n ((a,p):xxs) | r > p     = compRest r xxs
                               | otherwise = a
                               where r = n-p
        

likl :: Prob a -> Either a Rational
likl = (foldr f (Right 1)) . getProb
  where f _     (Left  a) = Left a
        f (e,p) (Right n) | r > p     = Right r
                          | otherwise = Left e
                          where r = n - p
                          
instance F.Foldable Prob where
  foldr f i (Prob []        ) = i
  foldr f i (Prob ((x,_):xs)) = f x (F.foldr f i (Prob xs))


