module Key
    ( Key
    , KeyNumber(..)
    , KeyType(..)
    , add
    , increment
    , decrement
    , relative
    , compatibleKeys
    ) where

----------------------------------
----- Types ----------------------
----------------------------------

type Key = (KeyNumber, KeyType)

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
    deriving (Eq, Ord, Enum, Bounded, Show)

data KeyType
    = A
    | B
     deriving (Eq, Ord, Enum, Bounded, Show)

----------------------------------
----- Public Functions  ----------
----------------------------------

add :: Int -> Key -> Key
add n (keyNumber, keyType) = (incrementKeyNumber n keyNumber, keyType)

relative :: Key -> Key
relative (keyNumber, A) = (keyNumber, B)
relative (keyNumber, B) = (keyNumber, A)

increment :: Key -> Key
increment = add 1

decrement :: Key -> Key
decrement = add (-1)

compatibleKeys :: Key -> [Key]
compatibleKeys k = [increment k, decrement k, relative k]

----------------------------------
----- Internal Functions  --------
----------------------------------

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k =
    toEnum ((fromEnum k + n) `mod` 12)
