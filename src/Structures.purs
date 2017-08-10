module Structures where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Foldable (sum)
import Data.Array hiding ((:))
import Data.Int (hexadecimal, toStringAs)
import Data.String as Str
import Data.List ((:))
import Data.List as List
import Data.List.Types (List(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Partial.Unsafe (unsafeCrashWith)

data Colour = Colour Int Int Int
derive instance genericColour :: Generic Colour _
instance showColour :: Show Colour where show x = genericShow x

data Type = IntType | ColourType
derive instance eqType :: Eq Type
instance showType :: Show Type where
  show IntType = "Int"
  show ColourType = "Colour"

data Val = IntVal Int | ColourVal Colour
derive instance genericVal :: Generic Val _
instance showVal :: Show Val where show x = genericShow x

red :: Colour -> Int
red (Colour r g b) = r
green :: Colour -> Int
green (Colour r g b) = g
blue :: Colour -> Int
blue (Colour r g b) = b

data Block = Block
                String
                (Array Val -> Val) -- underlying function
                Type -- return type
                (Array Type) -- input types
instance showBlock :: Show Block where
    show (Block name f typ types) = "Block { " <> name <> " :: " <> show types <> " -> " <> show typ <> " }"

data Block' = Block'
                Block -- block itself
                (Array (Maybe Block')) -- blocks connected
derive instance genericBlock' :: Generic Block' _
instance showBlock' :: Show Block' where show x = genericShow x

getBlock :: Block' -> Block
getBlock (Block' block _) = block

getBlockTypes :: Block' -> Array Type
getBlockTypes (Block' (Block name f out ins) _) = ins

getFn :: Block' -> (Array Val -> Val)
getFn (Block' (Block name f out ins) _) = f

getRet :: Block' -> Type
getRet (Block' (Block name f out ins) _) = out

getName :: Block' -> String
getName (Block' (Block name f out ins) _) = name

getWidth :: Maybe Block' -> Int
getWidth Nothing = 1
getWidth (Just (Block' _ [])) = 1
getWidth (Just (Block' _ connected)) = sum (map getWidth connected)

idBlock :: Type -> Block
idBlock typ = Block "id" f typ [typ]
  where
    f :: Array Val -> Val
    f [val] = val
    f _ = unsafeCrashWith "Wrong number of inputs"

idBlock' :: Type -> Block'
idBlock' typ = Block' (idBlock typ) [Nothing]

instance valBlockInt :: ValBlock Int where
  valBlock i = Block (show i) (const (IntVal i)) IntType []

instance valBlockColour :: ValBlock Colour where
  valBlock c = Block (colourToHex c) (const (ColourVal c)) ColourType []

colourBlock :: Int -> Int -> Int -> Block
colourBlock r g b = valBlock (Colour r g b)

colourBlock' :: Int -> Int -> Int -> Block'
colourBlock' r g b = valBlock' (Colour r g b)

addBlock :: Block
addBlock = Block "(+)" f IntType [IntType, IntType]
  where
    f :: Array Val -> Val
    f [IntVal a, IntVal b] = IntVal $ a + b
    f _ = unsafeCrashWith "Adding wrong things"

initBlock :: Block -> Block'
initBlock block@(Block _ _ _ inputTypes) =
    Block' block (replicate (length inputTypes) Nothing)

blueBlock :: Block
blueBlock = Block "blue" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ blue a
    f _ = unsafeCrashWith "Not getting blue from a colour"

greenBlock :: Block
greenBlock = Block "green" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ green a
    f _ = unsafeCrashWith "Not getting green from a colour"

redBlock :: Block
redBlock = Block "red" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ red a
    f _ = unsafeCrashWith "Not getting red from a colour"

evaluate :: Block' -> Maybe Val
evaluate (block'@(Block' (Block name fn ret ins) [])) = Just $ fn []
evaluate (block'@(Block' (Block name fn ret ins) connected))
  | all isJust connected = evaluate (joinBlocks "" block')
  | otherwise            = Nothing


type Coords = List Int

blockAt :: Coords -> Block' -> Maybe Block'
blockAt Nil block = Just block
blockAt (Cons x xs) (Block' _ connected) = blockAt xs =<< join (connected !! x)

isFree :: Coords -> Block' -> Maybe Boolean
isFree Nil _ = Nothing
isFree (Cons x Nil) (Block' _ connected) =
    do connectedBlock <- connected !! x
       pure $ isNothing connectedBlock
isFree (Cons x xs) (Block' _ connected) = isFree xs =<< join (connected !! x)

findBlock :: Coords -> Maybe Block' -> Maybe Block'
findBlock Nil mBlock = mBlock
findBlock (Cons x xs) mBlock =
  do
    (Block' block connected) <- mBlock
    connectedBlock <- connected !! x
    findBlock xs $ connectedBlock

removeBlock :: Coords -> Maybe Block' -> Maybe Block'
removeBlock coords mMainBlock = updateBlock coords Nothing mMainBlock

insertBlock :: Coords -> Block' -> Maybe Block' -> Maybe Block'
insertBlock coords newBlock mMainBlock = updateBlock coords (Just newBlock) mMainBlock

updateBlock :: Coords -> Maybe Block' -> Maybe Block' -> Maybe Block'
updateBlock Nil newBlock mMainBlock = newBlock
updateBlock (Cons x xs) newBlock mMainBlock = 
  do (Block' block connected) <- mMainBlock
     connectedBlock <- connected !! x
     let innerBlock = updateBlock xs newBlock connectedBlock 
     connected' <- updateAt x innerBlock connected
     pure $ Block' block connected'

getType :: Coords -> Block' -> Maybe Type
getType (Cons x Nil) (Block' (Block _ _ _ types) _) = types !! x
getType Nil (Block' (Block _ _ out ins) _) = Just out
getType (Cons x xs) (Block' _ connected) = getType xs =<< join (connected !! x)

joinBlocks :: String -> Block' -> Block'
joinBlocks name' block'@(Block' (Block name fn output inputs) connected)
  | all isNothing connected = block'
  | otherwise = Block' (Block name' fn' output inputs') (map (const Nothing) inputs')
      where
        connectedTemp :: Array (Maybe Block')
        connectedTemp = map (map (joinBlocks "")) connected
        connected' :: Array Block'
        connected' = zipWith fillBlanks inputs connectedTemp
          where
            fillBlanks :: Type -> Maybe Block' -> Block'
            fillBlanks inputType b' = fromMaybe (idBlock' inputType) b'
        
        inputs' = concatMap getBlockTypes connected'
        splits = toUnfoldable $ map (length <<< getBlockTypes) connected'
        fns = toUnfoldable $ map getFn connected'
        -- {inputs', splits, fns} = foldr f {inputs': [], splits: Nil, fns: Nil} connected'
        --   where
        --     f block rec = {
        --         inputs': inputBlocks <> rec.inputs',
        --         splits: (length inputBlocks : rec.splits),
        --         fns: (getFn block : rec.fns)
        --     }
        --       where inputBlocks = getBlockTypes block
        fn' = \args -> fn <<< fromFoldable $ List.zipWith ($) fns (splitArgs splits args)

splitArgs :: forall a . List Int -> Array a -> List (Array a)
splitArgs Nil xs = Nil
splitArgs (s:ss) xs = take s xs : splitArgs ss (drop s xs)

colourToHex :: Colour -> String
colourToHex (Colour r g b) = "#" <> intToHex r <> intToHex g <> intToHex b
  where
    intToHex x = padHex $ toStringAs hexadecimal $ x `mod` 256
    padHex str
      | Str.length str == 1 = "0" <> str
      | otherwise           = str

class ValBlock a where
  valBlock :: a -> Block

valBlock' :: forall a . ValBlock a => a -> Block'
valBlock' = initBlock <<< valBlock
