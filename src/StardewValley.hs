{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeApplications #-}

module StardewValley where

import Data.List (intercalate, findIndices)
import Control.Monad (guard, ap)
import Text.Printf (printf)
import Data.Bifunctor (second)

data Place = InOcean | InFresh 
  deriving (Show, Eq, Enum, Bounded)

data Trash = Junk | Driftwood | Newspaper | CD | Glasses 
  deriving (Show, Eq, Enum, Bounded)
data Ocean = Lobster | Clam | Crab | Cockle | Mussel | Shrimp | Oyster 
  deriving (Show, Eq, Enum, Bounded)
data Fresh = Crayfish | Snail | Periwinkle
  deriving (Show, Eq, Enum, Bounded)
data RecyclingGood = Stone | Coal | IronOre | Wood | Torch | Cloth | Quarz
  deriving (Show, Eq, Enum, Bounded)

data Item = Trash (Int, RecyclingGood) | Ocean Ocean | Fresh Fresh
  deriving (Eq)

data Fishing = NoFishing | Fisher | Angler | Mariner | MarinerAngler  
  deriving (Show, Eq, Enum, Bounded)

data Farming = NoRecycling | NoFarming | Rancher | Artisan
  deriving (Show, Eq, Enum, Bounded)

instance Show Item where
  show (Trash t) = show t
  show (Ocean t) = show t
  show (Fresh t) = show t

data ProbTree d a = Node [(d, ProbTree d a)] | Leaf a
  deriving (Show, Foldable, Functor)

instance Applicative (ProbTree d) where
  pure  = Leaf
  (<*>) = ap

instance Monad (ProbTree d) where
  Leaf x >>= f = f x
  Node l >>= f = Node $ fmap (second (>>= f) ) l

-- | 'everything' is a little hack, to get a list of every constructor for a given type.
everything :: (Enum a, Bounded a) => [a]
everything = [minBound .. maxBound]

showTree :: (Show a, Show d, Fractional d) => ProbTree d a -> String
showTree = intercalate "\n" . map fst . showTree'
  where
    showTree' :: (Show a, Show d, Fractional d) => ProbTree d a -> [(String, Bool)]
    showTree' (Leaf x) = [(show x, False)]
    showTree' (Node l) = do
        (p,t) <- l
        let subLines = showTree' t
        let lastArrowLine = case findIndices snd subLines of
              [] -> 0
              l  -> last l
        let indent = zipWith (f lastArrowLine) [0..] subLines
        (show p, True) : indent
    f lastArrowLine _ (line, True) = ("+-->" ++ line, False)
    f lastArrowLine i (line, False)
      | i < lastArrowLine = ("|   " ++ line, False)
      | otherwise         = ("    " ++ line, False)

newtype LeafLine d a = LeafLine (d, a)

instance (Show a, Show d, RealFrac d) => Show (LeafLine d a) where
  show (LeafLine (p, x)) = show x ++ " => " ++ show p ++ " \8776 " ++ showPercentage p

showPercentage :: RealFrac d => d -> String
showPercentage d = (++"%") $ show $ round $ d * 100

showTreeProbs :: (Show a, Show d, RealFrac d) => ProbTree d a -> String
showTreeProbs t = showTree $ LeafLine <$> probabilityIntoLeafs t

evenDistributed :: Fractional d => [a] -> ProbTree d a
evenDistributed l = Node $ map ((probability, ) . Leaf ) l
  where
    probability = intRecip $ length l

intRecip :: Fractional d => Int -> d
intRecip = recip . fromInteger . toEnum

trashTree :: Fractional d => ProbTree d Trash
trashTree = evenDistributed everything

recyclingGoods :: Fractional d => Trash -> [(d, RecyclingGood)]
recyclingGoods Junk      = [(49/100, Stone),(30/100, Coal), (21/100, IronOre)]
recyclingGoods Driftwood = [(75/100, Wood),(25/100, Coal)]
recyclingGoods Newspaper = [(90/100, Torch),(10/100, Cloth)]
recyclingGoods CD        = [(1, Quarz)]
recyclingGoods Glasses   = [(1, Quarz)]

multiplesOfRecyclingGoods :: RecyclingGood -> [Int]
multiplesOfRecyclingGoods Stone   = [1,2,3] 
multiplesOfRecyclingGoods Coal    = [1,2,3]
multiplesOfRecyclingGoods IronOre = [1,2,3]
multiplesOfRecyclingGoods Wood    = [1,2,3]
multiplesOfRecyclingGoods Torch   = [3]
multiplesOfRecyclingGoods Cloth   = [1]
multiplesOfRecyclingGoods Quarz   = [1]

recycleTrashTree :: Fractional d => Trash -> ProbTree d (Int, RecyclingGood)
recycleTrashTree trash = do
    recyclingGood <- Node $ second Leaf <$> recyclingGoods trash
    let multiples = multiplesOfRecyclingGoods recyclingGood
    leaf <- Node $ (intRecip (length multiples),) <$> Leaf <$> (, recyclingGood) <$> multiples
    return leaf

recyclingGoodsTree :: Fractional d => ProbTree d (Int, RecyclingGood)
recyclingGoodsTree = trashTree >>= recycleTrashTree

splitTree :: Fractional d => d -> ProbTree d a -> ProbTree d a -> ProbTree d a
splitTree p t t' = Node [ (p, t), (1 - p, t') ]

chainTree :: Fractional d =>  ProbTree d a -> [(d,a)] -> ProbTree d a
chainTree = foldr (\(d, a) t -> splitTree d (Leaf a) t )

noMarinerTree :: Fractional d => (a -> Item) -> [(d,a)] -> ProbTree d Item
noMarinerTree f catchProbs = splitTree (80 / 100) catch trashT
  where
    trashT = fmap Trash recyclingGoodsTree
    catch = chainTree trashT $ second f <$> catchProbs


{- Fish.xnb
  "715": "Lobster/trap/.05/688 .45 689 .35 690 .35/ocean/2/20",
  "717": "Crab/trap/.1/684 .45/ocean/1/20",
  "723": "Oyster/trap/.15/682 .35/ocean/1/5",
  "372": "Clam/trap/.15/681 .35/ocean/1/5",
  "720": "Shrimp/trap/.2/681 .35/ocean/1/3",
  "718": "Cockle/trap/.3/680 .2/ocean/1/5",
  "719": "Mussel/trap/.35/683 .15/ocean/1/5",
  "721": "Snail/trap/.25/680 .35/freshwater/1/5",
  "716": "Crayfish/trap/.35/682 .4/freshwater/1/8",
  "722": "Periwinkle/trap/.55/681 .35/freshwater/1/3",
-}

oceanTree :: Fractional d => ProbTree d Item
oceanTree = noMarinerTree Ocean [ ( 5 / 100, Lobster)
                                , (10 / 100, Crab)
                                , (15 / 100, Oyster)
                                , (15 / 100, Clam)
                                , (20 / 100, Shrimp)
                                , (30 / 100, Cockle)
                                , (35 / 100, Mussel)
                                ]

oceanTree' :: Fractional d => ProbTree d Item
oceanTree' = noMarinerTree Ocean [ (15 / 100, Clam)
                                , ( 5 / 100, Lobster)
                                , (10 / 100, Crab)
                                , (30 / 100, Cockle)
                                , (35 / 100, Mussel)
                                , (20 / 100, Shrimp)
                                , (15 / 100, Oyster)
                                ]

freshTree :: Fractional d => ProbTree d Item
freshTree = noMarinerTree Fresh [ (25 / 100, Snail)
                                , (35 / 100, Crayfish)
                                , (55 / 100, Periwinkle)
                                ]

freshTree' :: Fractional d => ProbTree d Item
freshTree' = noMarinerTree Fresh [ (35 / 100, Crayfish)
                                , (25 / 100, Snail)
                                , (55 / 100, Periwinkle)
                                ]

probabilityIntoLeafs :: Fractional d => ProbTree d a -> ProbTree d (d, a)
probabilityIntoLeafs = probabilityIntoLeafs' 1
  where
    probabilityIntoLeafs' :: Fractional d => d -> ProbTree d a -> ProbTree d (d, a)
    probabilityIntoLeafs' p (Leaf a) = Leaf (p, a)
    probabilityIntoLeafs' p (Node l) = Node $ map (\(d, t') -> (d, probabilityIntoLeafs' (p * d) t')) l

probabilityOfItem :: (Eq a, Fractional d) => ProbTree d a -> a -> d
probabilityOfItem tree x = eventProbability tree (==x)

eventProbability :: Fractional d => ProbTree d a -> (a -> Bool) -> d
eventProbability tree f = sum $ filter <$> probabilityIntoLeafs tree
  where
    filter (p, x)
      | f x = p
      | otherwise = 0

avarage :: Fractional d => ProbTree d d -> d
avarage t = sum $ uncurry (*) <$> probabilityIntoLeafs t

getProbabilityTree :: Fractional d => Place -> Fishing -> ProbTree d Item
getProbabilityTree place skill = case (isMariner, place) of
        (False, InOcean) -> oceanTree
        (False, InFresh) -> freshTree
        (True, InOcean)  -> fmap Ocean $ evenDistributed $ (everything :: [Ocean])
        (True, InFresh)  -> fmap Fresh $ evenDistributed $ (everything :: [Fresh])
  where
    isMariner = elem skill [Mariner, MarinerAngler]

-- | This Function checks if a certain Item can be converted into sashimi.
isFish :: Item -> Bool
isFish (Trash _) = False
isFish (Ocean Clam) = False
isFish _ = True

recyclingGoodValue :: Integral i => Farming -> RecyclingGood -> i
recyclingGoodValue NoRecycling _ = 0
recyclingGoodValue _ Stone   = 2
recyclingGoodValue _ Coal    = 15
recyclingGoodValue _ IronOre = 10
recyclingGoodValue _ Wood    = 2
recyclingGoodValue _ Torch   = 5 
recyclingGoodValue _ Quarz   = 50
recyclingGoodValue NoFarming Cloth = 470
recyclingGoodValue Rancher Cloth = 564
recyclingGoodValue Artisan Cloth = 658

baseValue :: Integral i => Item -> i 
baseValue ( Trash _ ) = 0
baseValue (Ocean o) = case o of
    Lobster -> 120
    Clam   -> 50
    Crab   -> 100
    Cockle -> 50
    Mussel -> 30
    Shrimp -> 60
    Oyster -> 40
baseValue (Fresh f) = case f of
    Crayfish   -> 75
    Snail      -> 65
    Periwinkle -> 20

profit :: Integral i => Fishing -> Farming -> Bool -> Item -> i
profit fi fa sashimi item = case item of
  Trash (n,x) -> toEnum n * recyclingGoodValue fa x
  _       -> intoSashimi $ applyFishingProfession $ baseValue item
  where
    intoSashimi :: Integral i => i -> i
    intoSashimi
        | sashimi && isFish item = max 75
        | otherwise              = id
    applyFishingProfession :: Integral i => i -> i
    applyFishingProfession = case (isFish item, fi) of
        (False, _)  -> id
        (_, Fisher) -> \i -> div (i * 5) 4
        (_, Angler) -> \i -> div (i * 3) 2
        (_, MarinerAngler) -> \i -> div (i * 3) 2
        _ -> id

profitsIntoLeafs :: Integral i => Fishing -> Farming -> Bool -> ProbTree d Item -> ProbTree d (Item, i)
profitsIntoLeafs fi fa sashimi = fmap (\item -> (item, profit fi fa sashimi item))

avarageProfit :: (Fractional d) => Fishing -> Farming -> Bool -> ProbTree d Item -> d
avarageProfit fi fa sashimi tree = avarage $ fromInteger . snd <$> profitsIntoLeafs fi fa sashimi tree

trimShow :: Show a => Int -> a -> String
trimShow n str = take n $ show str ++ repeat ' '

table :: String
table = intercalate "\n" $ do
  place <- everything
  sashimi <- everything
  fishing <- everything
  let farming = NoRecycling
  let tree = getProbabilityTree place fishing :: ProbTree Rational Item
  let profit = avarageProfit fishing farming sashimi tree :: Rational
  return $ intercalate " " $ [trimShow 7 place, trimShow 10 fishing, trimShow 10 farming, trimShow 5 sashimi, roundFraction 2 profit, show profit]

tableRecyclings :: String
tableRecyclings = intercalate "\n" $ do
  place <- everything
  sashimi <- everything
  fishing <- everything
  let tree = getProbabilityTree place fishing :: ProbTree Rational Item
  let profit f = avarageProfit fishing f sashimi tree :: Rational
  let profits = roundFraction 2 <$> subtract (profit NoRecycling) <$> profit <$> [NoRecycling, NoFarming, Rancher, Artisan]
  return $ intercalate " " $ [trimShow 7 place, trimShow 10 fishing, trimShow 5 sashimi] ++ profits

roundFraction :: (RealFrac d,Show d) => Int -> d -> String
roundFraction n f
  | length str <= n = str
  | otherwise       = insertAt (length str - n) '.' str
  where
    str = show $ round $ f * 10^n
    insertAt :: Int -> a -> [a] -> [a]
    insertAt 0 a l = a : l
    insertAt i a [] = []
    insertAt i a (x:xs) = x : insertAt (i - 1) a xs


-- NoFishing | Fisher | Angler | Mariner | MarinerAngler  
