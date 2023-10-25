module Chapter8.MyDataType (runChapter8) where

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- Record Syntax
data Person = Person
  { firseName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

alice =
  Person
    { firseName = "alice",
      lastName = "bob",
      age = 100,
      height = 200,
      phoneNumber = "300",
      flavor = "400"
    }

runPrintAlice = print alice

-- Type parameters
data Car a b c = Car
  { company :: a,
    model :: b,
    year :: c
  }
  deriving (Show)

data V3 a = V3 a a a deriving (Show)

v3plus :: (Num t) => V3 t -> V3 t -> V3 t
(V3 i j k) `v3plus` (V3 l m n) = V3 (i + l) (j + m) (k + n)

runV3Plus = print $ V3 1 2 3 `v3plus` V3 4 5 6

-- Type synonyms

-- `type` actually is `alias`
-- `data` actually is definiting a new "class"
type MyChar = Char

type MyString = [MyChar]

-- Type alias with parameters
type AssocList k v = [(k, v)]

runChapter8 :: IO ()
runChapter8 = runV3Plus
