{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module IntList where

import MD5Tag
import Tagable
import Patchable
import Doc

import Control.Applicative
import Data.Maybe
import Test.QuickCheck
import qualified Data.Binary.Put as Bin

import Debug.Trace

newtype IntList = IntList [Int]
    deriving (Show, Eq, Arbitrary)

-- instead of storing whole structure just store enough info to do inverses
instance Patchable IntList where
    data Mod IntList = Insert Int (Maybe Int)
                     | Remove Int (Maybe Int)
        deriving (Show)

    act (Insert i ma) (IntList l)
      | i < 0 || i > length l = if isJust ma
                                then error "Invalid Insert mod"
                                else IntList l
      | otherwise = case ma of
                      Nothing -> error "Invalid Insert mod"
                      Just a -> IntList $ take i l ++ a : drop i l
    act (Remove i ma) (IntList l)
      | i < 0 || i > length l - 1 = if isJust ma
                                    then error "Invalid Remove mod"
                                    else IntList l
      | otherwise = IntList $ take i l ++
                                   case (drop i l, ma) of
                                     ([], Nothing) -> []
                                     (a : as, Just a') -> if a /= a'
                                                          then error "Invalid Remove mod"
                                                          else as
                                     w -> error $ "Invalide Remove mod" ++ show w

    inverse (Insert _ Nothing) = []
    inverse (Remove _ Nothing) = []
    inverse (Insert i ma) = [Remove i ma]
    inverse (Remove i ma) = [Insert i ma]

    -- Remove _ Nothing = id
    merge m (Remove _ Nothing) = ([m], [])
    merge (Remove _ Nothing) m = ([], [m])
    merge m (Insert _ Nothing) = ([m], [])
    merge (Insert _ Nothing) m = ([], [m])
    merge (Insert i0 a) (Insert i1 b)
      | i0 == i1 && a == b = ([], [])
      | i0 < i1 = ([Insert i0 a], [Insert (i1 + 1) b])
      | otherwise = ([Insert (i0 + 1) a], [Insert i1 b]) -- asymmetric case when i0 == i1
    merge (Insert i0 a) (Remove i1 (Just b))
      | i0 > i1 = ([Insert (i0 - 1) a], [Remove i1 (Just b)])
      | otherwise = ([Insert i0 a], [Remove (i1 + 1) (Just b)])
    merge (Remove i1 (Just b)) (Insert i0 a) -- no conflict so no asymmetry, same as before
      | i0 > i1 = ([Remove i1 (Just b)], [Insert (i0 - 1) a])
      | otherwise = ([Remove (i1 + 1) (Just b)], [Insert i0 a])
    merge (Remove i0 (Just a)) (Remove i1 (Just b))
      | i0 == i1 = if a /= b
                   then error "Invalid Remove mod"
                   else ([], [])
      | i0 < i1 = ([Remove i0 (Just a)], [Remove (i1 - 1) (Just b)])
      | otherwise = ([Remove (i0 - 1) (Just a)], [Remove i1 (Just b)])

serialise :: Mod IntList -> Bin.Put
serialise (Insert i ma) = do
  Bin.putWord8 0
  Bin.putWord32le $ fromIntegral i
  case ma of
    Nothing -> Bin.putWord8 0
    Just a -> do
            Bin.putWord8 1
            Bin.putWord32le $ fromIntegral a
serialise (Remove i ma) = do
  Bin.putWord8 1
  Bin.putWord32le $ fromIntegral i
  case ma of
    Nothing -> Bin.putWord8 0
    Just a -> do
            Bin.putWord8 1
            Bin.putWord32le $ fromIntegral a

instance Tagable IntList where
    data Tag IntList = IntListTag MD5Tag
        deriving (Show, Eq, Ord)

    initTag = IntListTag emptyMD5Tag

    alpha [] t = t
    alpha ms (IntListTag t) = IntListTag . md5Tag $ do
      Bin.putByteString (rawMD5 t)
      mapM_ serialise ms

    beta (IntListTag t0) (IntListTag t1) = IntListTag . md5Tag $ do
      Bin.putByteString (rawMD5 t0)
      Bin.putByteString (rawMD5 t1)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a : _) = Just a

instance GenPatch IntList where
    genPatch d@(IntList l) =
      frequency [ (7, return [])
                , (9, do
                     m <- oneof [genRemove, genInsert]
                     ms <- genPatch $ act m d
                     return $ m : ms)
                ]
      where
        genRemove =
          frequency [ (3, do
                         i <- if null l then return 0 else choose (0, length l - 1)
                         return $ Remove i (headMaybe $ drop i l))
                    , (1, do
                         i <- arbitrary
                         return $ if i < 0
                                  then Remove i Nothing
                                  else Remove i (headMaybe $ drop i l))
                    ]
        genInsert = do
          frequency [ (3, do
                         i <- if null l then return 0 else choose (0, length l)
                         a <- arbitrary
                         return $ Insert i (Just a))
                    , (1, do
                         i <- arbitrary
                         if i < 0 || i > length l
                         then return $ Insert i Nothing
                         else do
                           a <- arbitrary
                           return $ Insert i (Just a))
                    ]

instance Doc IntList where
    initDoc = IntList []
