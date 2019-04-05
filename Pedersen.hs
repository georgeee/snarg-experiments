{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Math.NumberTheory.Primes
import Data.Bits
import Prelude ()
import Universum hiding (group)
import Control.Monad.Trans.Random.Strict (Rand, evalRand)
import Control.Monad.Random.Class (getRandomR)
import System.Random (StdGen, mkStdGen)
import Math.NumberTheory.Euclidean (extendedGCD)

-- | Triple of numbers where
-- Zp* is a group of prime order p
-- Gq being a cyclic subgroup of prime order q
-- g \in Gq is some generator for Gq
data PGroup = PGroup
  { p :: !Integer
  , q :: !Integer
  }
  deriving Show

mkPGroup :: Integer -> Maybe PGroup
mkPGroup p
  | not (isPrime p) || not (isPrime q) = Nothing
  | otherwise = Just (PGroup {..})
  where
    q = (p - 1) `div` 2

iteratePGroup :: PGroup -> [Integer]
iteratePGroup PGroup {..} = 1 :
  takeWhile (/=1) (iterate (\x -> x `seq` x * g `mod` p) g)
  where
    g = 4

randPGroup :: Int -> Rand StdGen PGroup
randPGroup pow =
  maybe (randPGroup pow) pure =<< mkPGroup <$> pR
  where
    e = 2 ^ (pow - 1)
    pR = (e .|.) <$> getRandomR (1, e - 1)

data DH = DH
  { group :: !PGroup
  , g :: !Integer
  , h :: !Integer
  }
  deriving Show

randPGroupEl :: PGroup -> [Integer] -> Rand StdGen Integer
randPGroupEl pg@(PGroup {..}) excludes = do
  r_ <- getRandomR (2, p - 1)
  let r = (r_ * r_) `mod` p
  if r `elem` excludes || r == 1
    then randPGroupEl pg excludes
    else pure r

randDH :: Int -> Rand StdGen DH
randDH pow = do
  group <- randPGroup pow
  g <- randPGroupEl group []
  h <- randPGroupEl group [g]
  pure DH {..}

newtype Commitment = Commitment Integer
  deriving Show

data Opening = Opening
  { opSecret :: !Integer
  , opSeed :: !Integer
  }
  deriving Show

commit :: DH -> Opening -> Commitment
commit DH {..} (Opening secret seed)
  | secret < 0 || secret >= q group =
      error $ "Unexpected secret" <> show secret <> ": must be in range [0.."
              <> show (q group) <>" - 1]"
  | seed < 0 || seed >= q group =
      error $ "Unexpected seed" <> show seed <> ": must be in range [0.."
              <> show (q group) <>" - 1]"
  | otherwise =
      let comm_ = (g ^ secret) * (h ^ seed) `mod` p group
       in comm_ `seq` Commitment comm_

randCommit :: DH -> Integer -> Rand StdGen (Commitment, Opening)
randCommit dh opSecret = do
  opSeed <- getRandomR (0, q (group dh) - 1)
  let open = Opening {..}
  pure (commit dh open, open)

randSecretAndCommit :: DH -> Rand StdGen (Commitment, Opening)
randSecretAndCommit dh = randCommit dh =<< getRandomR (0, q (group dh) - 1)

-- | Config for sharing secret to N parties
-- with K parties sufficient to recover the secret.
data ShareConfig = ShareConfig
  { scK :: !Int
  , scN :: !Int
  }
  deriving Show

data ShareData = ShareData
  { sdBasic  :: !(Commitment, Opening)
    -- ^ commitment and opening to secret @s@ and random value @t@
  , sdCoefs  :: ![(Commitment, Opening)]
    -- ^ list of commitments and openings for random
    -- coefficients @F_i@ and @G_i@ for @i := 1..k-1@
  , scShares :: ![Opening]
    -- ^ list of shares, @s_i@ and @t_i@ for @i := 1 .. n@
  }
  deriving Show

-- | Commitment list of length k.
-- Head element is commitment to @(s, t)@ for @s@ a secret.
-- Rest of elements correspond to coefficients of polynomial.
newtype ShareCommitment = ShareCommitment [Commitment]
  deriving Show

toShareCommit :: ShareData -> ShareCommitment
toShareCommit ShareData {..} =
  ShareCommitment (fst sdBasic : map fst sdCoefs)

newtype Poly = Poly [Integer]
  deriving Show

evalPoly
  :: Integer -- ^ Modulus
  -> Poly -- ^ Poly @F(x)@
  -> Integer -- ^ Point @x@
  -> Integer
evalPoly q (Poly coefs) x = modQ $
  foldr' (modQ ... (+)) 0 $ zipWith (modQ ... (*)) coefs (iterate (modQ . (* x)) 1)
  where
    modQ = (`mod` q)

randShareCommit :: DH -> ShareConfig -> Integer -> Rand StdGen ShareData
randShareCommit dh@(DH {..}) ShareConfig {..} secret = do
  sdBasic <- randCommit dh secret
  sdCoefs <- replicateM (scK - 1) (randSecretAndCommit dh)
  let fPoly = Poly $ opSecret . snd <$> sdBasic : sdCoefs
  let gPoly = Poly $ opSeed . snd <$> sdBasic : sdCoefs
  let scShares =
        map (\x -> Opening (evalPoly (q group) fPoly x)
                           (evalPoly (q group) gPoly x))
            [1 .. toInteger scN]
  pure ShareData {..}

randShuffle :: [a] -> Rand StdGen [a]
randShuffle lst = do
  coefs <- replicateM (length lst) $ getRandomR (minBound :: Int, maxBound)
  pure $ map snd $ sortOn fst $ zip coefs lst

validateOpening :: DH -> ShareCommitment -> Int -> Opening -> Bool
validateOpening dh (ShareCommitment comms) i open =
    expected == committed
  where
    PGroup {..} = group dh
    Commitment expected = commit dh open
    committed = foldr' combine 1 (zip comms [0..])
    combine (Commitment e, j) acc = acc * (e ^ pow) `mod` p
      where
        k = length comms
        pow = toInteger i ^ j `mod` q

reconstructSecret :: PGroup -> [(Int, Integer)] -> Integer
reconstructSecret PGroup {..} opens_ = foldr' combine 0 opens
  where
    opens = first toInteger <$> opens_
    ids = fst <$> opens
    combine (i, s) acc =
      (acc + s * calcA i) `mod` q
    calcA i = (`mod` q) $ foldr' ((*) . coefIJ i) 1 ids
    coefIJ i j
      | i == j = 1
      | otherwise = j * rev (j - i) q

rev x p =
  let (_, s, _) = extendedGCD x p
   in (s + p) `mod` p

shareExperiment :: Int -> ShareConfig -> Integer -> Rand StdGen Integer
shareExperiment pow sc@(ShareConfig{..}) secret = do
  dh <- randDH pow
  sd@(ShareData{..}) <- randShareCommit dh sc secret
  let comm = toShareCommit sd
      check = all (uncurry $ validateOpening dh comm) $ zip [1..] scShares
  when (not check) $ error "unexpectedly generated wrong openings"
  indexedOpens <- take scK <$> pure (zip [1..] (opSecret <$> scShares))
  pure $ reconstructSecret (group dh) indexedOpens

execExperiment :: Integer
execExperiment =
  evalRand (shareExperiment 17 (ShareConfig 30 50) 27622) (mkStdGen 1)
