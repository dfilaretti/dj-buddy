module Domain.Camelot (
    Key(..),
    KeyNumber(..),
    KeyType(..),
    compatibleKeys,
    increment,
    relative,
    incrementKeyNumber) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (FromJSON)
import Text.Read (readMaybe)

{---------------------------------------------------------------------------
  Data
----------------------------------------------------------------------------}

data Key = Key KeyNumber KeyType
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

{---------------------------------------------------------------------------
  Instances
----------------------------------------------------------------------------}

instance Show KeyNumber where
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
  show (Key n t) = show n ++ show t

instance Read KeyNumber where
  readsPrec precedence input =
    [ (toEnum (number - 1), rest)
    | (number, rest) <- (readsPrec precedence input :: [(Int, String)]),
      number >= 1,
      number <= 12
    ]

instance Read KeyType where
  readsPrec _ input =
    [(D, rest) | ("D", rest) <- lex input]
      ++ [(M, rest) | ("M", rest) <- lex input]

instance Read Key where
  readsPrec _ input =
    [ (Key number keyType, rest)
    | (number, keyTypeInput) <- reads input,
      (keyType, rest) <- reads keyTypeInput
    ]

instance FromJSON Key where
  parseJSON = Aeson.withText "Key" $ \s ->
    case parseKey s of
      Just key -> pure key
      Nothing -> fail $ "Invalid key: " ++ show s

parseKey :: Text -> Maybe Key
parseKey = readMaybe . Text.unpack

{---------------------------------------------------------------------------
  Internal functions
----------------------------------------------------------------------------}

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k =
  toEnum ((fromEnum k + n) `mod` 12)

add :: Int -> Key -> Key
add n (Key number keyType) =
  Key (incrementKeyNumber n number) keyType

relative :: Key -> Key
relative (Key n D) = Key n M
relative (Key n M) = Key n D

increment, decrement :: Key -> Key
increment = add 1
decrement = add (-1)

{---------------------------------------------------------------------------
  Exported functions
----------------------------------------------------------------------------}

compatibleKeys :: Key -> [Key]
compatibleKeys k =
  [increment k, decrement k, relative k]
