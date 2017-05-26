-- Name: Aditya Sharma
-- UID: u6051965
-- Collaborators: Jane Doe, Joe Bloggs
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  , example
  , solver
  ) where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List
import Data.String.Utils

example :: Sudoku
example =
  Sudoku
    [ [ Just 3
      , Just 6
      , Nothing
      , Nothing
      , Just 7
      , Just 1
      , Just 2
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 5
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 8
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 9
      , Just 2
      , Nothing
      , Just 4
      , Just 7
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 3
      , Nothing
      , Just 2
      , Just 8
      ]
    , [ Just 4
      , Nothing
      , Nothing
      , Just 5
      , Nothing
      , Just 2
      , Nothing
      , Nothing
      , Just 9
      ]
    , [ Just 2
      , Just 7
      , Nothing
      , Just 4
      , Just 6
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 5
      , Just 3
      , Nothing
      , Just 8
      , Just 9
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 8
      , Just 3
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 6
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 7
      , Just 6
      , Just 9
      , Nothing
      , Nothing
      , Just 4
      , Just 3
      ]
    ]

-- A matrix is a list of rows.
type Matrix a = [Row a]

-- A row is a list of values.
type Row a = [a]

-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int

-- A Sudoku puzzle is a matrix of cells.
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)

-- | cells extracts the cells from a Sudoku
cells (Sudoku m) = m



-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku
            $ replicate 9
            $ replicate 9 Nothing



-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) = all (==9) (map length $ s) == True
                      &&
                      length (map length $ s) == 9



-- | noBlanks checks if a Sudoku has no blanks
noBlanks :: Sudoku -> Bool
noBlanks (Sudoku s) = all (==0) [length $ filter (==Nothing) (concat $ s)]



-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- Example:
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3

-- | This helper function is used to generate a readable list from a given sudoku,
-- | This is especially helpful as we can use it to change the Maybe Types in a sudoku
-- | and then generate a readable representation
converter :: Sudoku -> [[Char]]
converter (Sudoku s) = map concat
                       $ chunksOf 9
                       $ map (replace "0" ".")
                       $ map show
                       $ map (fromMaybe 0)
                       $ concat
                       $ s

printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s) = mapM_ putStrLn
                         $ converter
                         $ Sudoku s



-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell =
  frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]



-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)



-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation

-- | length(string) != 81 = error "String is not a valid 9x9 Sudoku."
-- | otherwise = fromString (string)
--  prop> toString (fromString s) == s
fromString :: String -> Sudoku
fromString string
             | length string == 81 = Sudoku
                                     $ chunksOf 9
                                     $ maybeMapper
                                     $ string
             | otherwise = error "String is not a valid 9x9 Sudoku."

-- | Pretty much a reverse of the converter helper function,
-- | This one takes a given string representation
-- | And returns the valid sudoku Maybe type for each character in the string
maybeMapper :: String -> Row Cell
maybeMapper string = case string of
                     [] -> []
                     x:xs
                       | x == '.' -> ((Nothing :: Cell) : maybeMapper xs)
                       | otherwise -> ((Just (digitToInt x) :: Cell): maybeMapper xs)



-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
--  prop> fromString (toString s) == s
toString :: Sudoku -> String
toString (Sudoku s)
  | isSudoku (Sudoku s) = concat
                          $ converter
                          $ Sudoku s
  | otherwise = error "Not a valid 9x9 Sudoku."



type Block a = [a]

rows :: Matrix a -> [Block a]
rows a = a

cols :: Matrix a -> [Block a]
cols a = transpose a

boxs :: Matrix a -> [Block a]
boxs a = rows
         $ map concat
         $ concatMap (chunksOf 3)
         $ cols
         $ map (chunksOf 3) a



-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock block
    | (length
       $ filter (/=Nothing) block) /= (length
                                       $ nub
                                       $ filter (/=Nothing) block) = False
    | otherwise = True



-- | Check structure of a Sudoku: 9 rows, 9 columns, 9 boxes, each of
-- | exactly 9 cells
-- prop> prop_Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku s)
   | isSudoku (Sudoku s)
     &&
     (length $ boxs $ s) == 9
     &&
     all (==9) (map length $ boxs $ s) = True
   | otherwise = False



-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku s) = (blockCheck $ rows $ s)
                      &&
                      (blockCheck $ cols $ s)
                      &&
                      (blockCheck $ boxs $ s)
-- | Check if every block is viable in a given sudoku
-- | so that our okSudoku can be cleaner than just "all (==True) (map okBlock etc)"
blockCheck :: [Block Cell] -> Bool
blockCheck blocks = case blocks of
  [] -> True
  x:xs
    | okBlock x -> blockCheck xs
    | otherwise -> False



-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)

-- A position is the (column, row) position of a given cell.
type Pos = (Int, Int)

blank :: Sudoku -> Pos
blank (Sudoku s) = (yPos
                    $ map xChecker
                    $ s,
                    xIterator
                    $ map xChecker
                    $ s)

-- | helper to give indexes of Nothing in every row,
-- | if there isn't a Nothing val in the row, we get 11
xChecker :: [Cell] -> Int
xChecker row = fromMaybe 11 (elemIndex Nothing row)

-- | helper to take a list of xChecker values and
-- | return the first value that isn't 11 (i.e return the index of a valid nothing value)
xIterator :: [Int] -> Int
xIterator list = fromMaybe 11 (find (/=11) list)

-- | take the list of xChecker values and find the index for the first value that isnt 11
-- | this is our y value
yPos :: [Int] -> Int
yPos nums = case nums of
  [] -> error "Int out of pos."
  x:xs
    | x /= 11 -> fromMaybe 11 (elemIndex x nums)
    | otherwise -> (yCheck x) + (yPos xs)

-- | checker to make sure that y doesnt go out of index range
-- | if this function didnt exist, if y was 8 in yPos, we would return 9 and this would be out of index
yCheck :: Int -> Int
yCheck val
    | val == 8 = 0
    | otherwise = 1


-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, val) = take index list
                          ++
                          [val]
                          ++
                          drop (index+1) list



-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku s) (y,x) val = Sudoku
                              $ s !!= (y, (s !! y) !!= (x, Just val))



-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve string = solver
               $ fromString string

-- | backtracking solver
-- | brute force and check recursively with every value as something between 1 - 9
solver :: Sudoku -> [String]
solver s
  | not (okSudoku s) = []
  | noBlanks s = [toString s]
  | otherwise = concatMap (\val -> solver (update s (blank s) val)) [1..9]
