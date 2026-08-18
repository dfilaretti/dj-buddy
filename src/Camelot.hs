module Camelot
  ( Key (..),
    KeyNumber (..),
    KeyType (..),
    compatibleKeys,
  )
where

----------------------------------
----- Types ----------------------
----------------------------------

data Key
  = Key KeyNumber KeyType
  deriving (Eq)

data KeyNumber
  = K_1
  | K_2
  | K_3
  | K_4
  | K_5
  | K_6
  | K_7
  | K_8
  | K_9
  | K_10
  | K_11
  | K_12
  deriving (Eq, Ord, Enum, Bounded)

data KeyType
  = D
  | M
  deriving (Eq, Ord, Enum, Bounded, Show)

----------------------------------
----- Instances  -----------------
----------------------------------

instance Show KeyNumber where
  -- show :: KeyNumber -> String
  show K_1 = "1"
  show K_2 = "2"
  show K_3 = "3"
  show K_4 = "4"
  show K_5 = "5"
  show K_6 = "6"
  show K_7 = "7"
  show K_8 = "8"
  show K_9 = "9"
  show K_10 = "10"
  show K_11 = "11"
  show K_12 = "12"

instance Show Key where
  -- show :: Key -> String
  show (Key n t) = show n ++ show t

----------------------------------
----- Public Functions  ----------
----------------------------------

compatibleKeys :: Key -> [Key]
compatibleKeys k = [increment k, decrement k, relative k]

----------------------------------
----- Internal Functions  --------
----------------------------------

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k =
  toEnum ((fromEnum k + n) `mod` 12)

add :: Int -> Key -> Key
add n (Key keyNumber keyType) = Key (incrementKeyNumber n keyNumber) keyType

relative :: Key -> Key
relative (Key keyNumber D) = Key keyNumber M
relative (Key keyNumber M) = Key keyNumber D

increment :: Key -> Key
increment = add 1

decrement :: Key -> Key
decrement = add (-1)
