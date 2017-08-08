module Structures where

import Prelude
import Data.Exists
import Data.Maybe
import Partial.Unsafe (unsafeCrashWith)
import Data.Foldable (foldr,sum)
import Data.Array hiding ((:))
import Data.List ((:))
import Data.List as List
import Data.List.Types (List(..))
import Data.Show
import Data.Generic.Rep
import Control.Bind
import Data.Generic.Rep.Show
import Data.Typeable
import Data.Typelevel.Undefined
import Data.Tuple.Nested

type Colour = Tuple3 Int Int Int
data Type = IntType | ColourType
data Val = IntVal Int | ColourVal Colour

derive instance genericType :: Generic Type _
derive instance genericVal :: Generic Val _
derive instance genericBlock' :: Generic Block' _
instance showType :: Show Type where show x = genericShow x
instance showVal :: Show Val where show x = genericShow x
instance showBlock :: Show Block where
    show (Block name f typ types) = "Block { " <> name <> " :: " <> show types <> " -> " <> show typ <> " }"
instance showBlock' :: Show Block' where show x = genericShow x

data Block = Block
                String
                (Array Val -> Val) -- underlying function
                Type -- return type
                (Array Type) -- input types

data Block' = Block'
                Block -- block itself
                (Array (Maybe Block')) -- blocks connected

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

getHeight :: Maybe Block' -> Int
getHeight Nothing = 1
getHeight (Just (Block' _ [])) = 1
getHeight (Just (Block' _ connected)) = sum (map getHeight connected)

idBlock :: Type -> Block
idBlock typ = Block "id" f typ [typ]
  where
    f :: Array Val -> Val
    f [val] = val
    f _ = unsafeCrashWith "Wrong number of inputs"

idBlock' :: Type -> Block'
idBlock' typ = Block' (idBlock typ) [Nothing]

intBlock' :: Int -> Block'
intBlock' i = Block' (Block (show i) (const (IntVal i)) IntType []) []

addBlock :: Block
addBlock = Block "(+)" f IntType [IntType, IntType]
  where
    f :: Array Val -> Val
    f [IntVal a, IntVal b] = IntVal $ a + b
    f _ = unsafeCrashWith "Adding wrong things"

newBlock' :: Block -> Block'
newBlock' block@(Block _ _ _ inputTypes) =
    Block' block (replicate (length inputTypes) Nothing)

getBlueBlock :: Block
getBlueBlock = Block "Get Blue" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ get3 a
    f _ = unsafeCrashWith "Not getting blue from a colour"

getGreenBlock :: Block
getGreenBlock = Block "Get Green" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ get2 a
    f _ = unsafeCrashWith "Not getting green from a colour"

getRedBlock :: Block
getRedBlock = Block "Get Red" f IntType [ColourType]
  where 
    f :: Array Val -> Val
    f [ColourVal a] = IntVal $ get1 a
    f _ = unsafeCrashWith "Not getting red from a colour"

evaluate :: Block' -> Maybe Val
evaluate (block'@(Block' (Block name fn ret ins) [])) = Just $ fn []
evaluate (block'@(Block' (Block name fn ret ins) connected))
  | all isJust connected = evaluate (joinBlocks "" block')
  | otherwise            = Nothing


type Coords = List Int

isFree :: Coords -> Block' -> Maybe Boolean
isFree Nil _ = Nothing
isFree (Cons x Nil) (Block' _ connected) =
    do connectBlock <- connected !! x
       pure $ isNothing connectBlock
isFree (Cons x xs) (Block' _ connected) = isFree xs =<< join (connected !! x)

removeBlock :: Coords -> Maybe Block' -> Maybe Block'
removeBlock coords mMainBlock = updateBlock coords Nothing mMainBlock

insertBlock :: Coords -> Block' -> Maybe Block' -> Maybe Block'
insertBlock coords newBlock mMainBlock = updateBlock coords (Just newBlock) mMainBlock

updateBlock :: Coords -> Maybe Block' -> Maybe Block' -> Maybe Block'
updateBlock Nil newBlock mMainBlock = newBlock
updateBlock (Cons x xs) newBlock mMainBlock = 
  do (Block' block connected) <- mMainBlock
     connectBlock <- connected !! x
     let innerBlock = updateBlock xs newBlock connectBlock 
     connected' <- updateAt x innerBlock connected
     pure $ Block' block connected'

getType :: Coords -> Block' -> Maybe Type
getType (Cons x Nil) (Block' (Block _ _ _ types) connected) =
    case connected !! x of
        Nothing -> types !! x
        (Just x) -> Nothing
getType Nil _ = Nothing
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
            fillBlanks inputType block' = fromMaybe (idBlock' inputType) block'
        
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
