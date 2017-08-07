module Structures where

import Prelude
import Data.Exists
import Data.Maybe
import Partial.Unsafe (unsafeCrashWith)
import Data.Foldable (foldr)
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
    show (Block f typ types) = "Block { f :: " <> show types <> " -> " <> show typ <> " }"
instance showBlock' :: Show Block' where show x = genericShow x

data Block = Block
                    (Array Val -> Val) -- underlying function
                    Type -- return type
                    (Array Type) -- input types

data Block' = {-ValueBlock' Val
              |-} Block'
                    Block -- block itself
                    (Array (Maybe Block')) -- blocks connected

getBlock :: Block' -> Block
getBlock (Block' block _) = block

getBlockTypes :: Block' -> Array Type
getBlockTypes (Block' (Block f out ins) _) = ins

getFn :: Block' -> (Array Val -> Val)
getFn (Block' (Block f out ins) _) = f

getRet :: Block' -> Type
getRet (Block' (Block f out ins) _) = out

idBlock :: Type -> Block
idBlock typ = Block f typ [typ]
  where
    f :: Array Val -> Val
    f [val] = val
    f _ = unsafeCrashWith "Wrong number of inputs"

idBlock' :: Type -> Block'
idBlock' typ = Block' (idBlock typ) [Nothing]

const :: forall a b . a -> b -> a
const a b = a

intBlock' :: Int -> Block'
intBlock' i = Block' (Block (const (IntVal i)) IntType []) []

addBlock :: Block
addBlock = Block f IntType [IntType, IntType]
  where
    f :: Array Val -> Val
    f [IntVal a, IntVal b] = IntVal $ a + b
    f _ = unsafeCrashWith "Adding wrong things"

evaluate :: Block' -> Maybe Val
evaluate (block'@(Block' (Block fn ret ins) [])) = Just $ fn []
evaluate (block'@(Block' (Block fn ret ins) connected))
  | all isJust connected = evaluate (joinBlocks block')
  | otherwise            = Nothing


type Coords = List Int

isFree :: Coords -> Block' -> Maybe Boolean
isFree Nil _ = Nothing
isFree (Cons x Nil) (Block' _ connected) =
    do connectBlock <- connected !! x
       pure $ isNothing connectBlock
isFree (Cons x xs) (Block' _ connected) = isFree xs =<< join (connected !! x)
    

insertBlocks :: Coords -> Block' -> Maybe Block' -> Maybe Block'
insertBlocks Nil newBlock mMainBlock = Just newBlock
insertBlocks (Cons x xs) newBlock mMainBlock = 
  do (Block' block connected) <- mMainBlock
     connectBlock <- connected !! x
     innerBlock <- insertBlocks xs newBlock connectBlock 
     connected' <- updateAt x (Just innerBlock) connected
     pure $ Block' block connected'

getType :: Coords -> Block' -> Maybe Type
getType (Cons x Nil) (Block' (Block _ _ types) connected) =
    case connected !! x of
        Nothing -> types !! x
        (Just x) -> Nothing
getType Nil _ = Nothing
getType (Cons x xs) (Block' _ connected) = getType xs =<< join (connected !! x)

joinBlocks :: Block' -> Block'
--joinBlocks block'@(ValueBlock' _)       = block'
joinBlocks block'@(Block' (Block fn ret ins) connected)
  | all isNothing connected = block'
  | otherwise = 
      let
        connectedTemp :: Array (Maybe Block')
        connectedTemp = map (map (joinBlocks)) connected
        connected' :: Array Block'
        connected' = zipWith f ins connectedTemp
          where
            f :: Type -> Maybe Block' -> Block'
            f inputType block' = fromMaybe (idBlock' inputType) block'
        
        -- types = concatMap getBlockTypes connected'
        -- splits = map (length <<< getBlockTypes) connected'
        -- fns = map getFn connected'
        {types, splits, fns} = foldr f {types: [], splits: Nil, fns: Nil} connected'
          where
            f block rec = {
                types: inputBlocks <> rec.types,
                splits: (length inputBlocks : rec.splits),
                fns: (getFn block : rec.fns)
            }
              where inputBlocks = getBlockTypes block
        function = \args ->
          let
            splittedArgs = splitArgs splits args
          in
            fn <<< fromFoldable $ List.zipWith ($) fns splittedArgs
      in
        Block' (Block function ret types) (replicate (length types) Nothing)
        
splitArgs :: forall a . List Int -> Array a -> List (Array a)
splitArgs Nil xs = Nil
splitArgs (s:ss) xs = take s xs : splitArgs ss (drop s xs)

-- extractInt :: Val -> Int
-- extractInt (IntVal int) = int
-- extractInt _ = unsafeCrashWith "Cannot extract a not-int"
    
{-
data ValueBlock a = IntBlock Int
data FunctionBlock a c = IdBlock (ValueBlock a)
                       | FunctionBlock (ValueBlock a -> IdExistsBlock c)
                               --| FunctionBlock (ValueBlock a -> FunctionBlock a a)

oneIntFunctionBlock :: (Int -> Int) -> FunctionBlock Int Int
oneIntFunctionBlock f = FunctionBlock (\(IntBlock a) -> IdExistsBlock (mkExists (Flip (?1))))

intFunctionBlock :: forall a b. (Int -> FunctionBlock a b) -> FunctionBlock Int b
intFunctionBlock f = FunctionBlock (\(IntBlock a) -> IdExistsBlock (mkExists (Flip (f a))))

data Flip d a b = Flip (d b a)
unflip (Flip x) = x

data IdExistsBlock b = IdExistsBlock (Exists (Flip FunctionBlock b))

-- instance functorBlock :: Functor Block where
--     -- fmap :: (a->b) -> Block a -> Block b
--     fmap f (ValueBlock x)    = ValueBlock (f x)
--     fmap f (FunctionBlock x) = FunctionBlock (f x)

intBlock :: Int -> ValueBlock Int
intBlock int = IntBlock int

addBlock :: FunctionBlock Int Int
addBlock = intFunctionBlock (\a -> oneIntFunctionBlock (\b -> a+b))
--addBlock = FunctionBlock (\(IntBlock a) -> IdExistsBlock (mkExists (Flip (oneIntFunctionBlock (\b -> a+b)))))
-- addBlock = intFunctionBlock (\a -> oneIntFunctionBlock (\b -> a + b))
-}