module BlockViewer where

import Structures
import Helpers

import Prelude
import Data.Int (toNumber, hexadecimal, fromStringAs, fromString)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr)
import Data.Traversable (traverse_)
import Data.Array (filter, length, null, snoc, toUnfoldable, zip, zipWith, (..))
import Data.String as Str
import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Partial.Unsafe (unsafeCrashWith)
import Debug.Trace

import DOM (DOM)
import DOM.Event.Types (Event, MouseEvent, FocusEvent, mouseEventToEvent, focusEventToEvent)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation, preventDefault)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import DOM.HTML.Indexed.InputType (InputType(InputNumber, InputColor))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HCore
import Halogen.HTML.CSS (style)
import CSS.Stylesheet (CSS)
import CSS.Background (backgroundColor)
import CSS.Flexbox (flexGrow)
import Color as Color

type State = {
  structure :: Maybe Block',
  dragSource :: Maybe (Array Int),
  dragTarget :: Maybe (Array Int)
}

data BlockView = BlockView (Array Int) Block' | InputBlock (Array Int) Type

data Query a
  = IsEmpty (Boolean -> a)
  | Remove (Array Int) a
  | Drag (Array Int) DragEvent a
  | Drop (Array Int) a
  | DropIfBlank a
  | DragOverIfBlank DragEvent a
  | DropBlock Block' (Boolean -> a)
  | RemoveDragged (Unit -> a)
  | GetDragged (Maybe Block' -> a)
  | DragEnd a
  | DragOver DragEvent a
  | ChangeVal (Array Int) Event a
  | ResetVal (Array Int) FocusEvent a
  | Join a
  | StopPropagation MouseEvent a

myBlockViewer :: forall eff. H.Component HH.HTML Query Unit Message (Aff (dom :: DOM | eff))
myBlockViewer =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {
    structure: Just $ Block' addBlock
                    [Just (valBlock' 1),Just (Block' addBlock [Just (valBlock' 1), Just (Block' redBlock [Just $ colourBlock' 127 0 255])])],
    dragSource: Nothing,
    dragTarget: Nothing
  }
  
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [classes ["viewer"], HE.onDrop (HE.input_ DropIfBlank), HE.onDragOver (HE.input DragOverIfBlank)]
    [ HH.div [classes ["result"]] [HH.text $ maybe "" show evaluated]
    , structureRendering state
    ]
    where
      evaluated = evaluate =<< state.structure
  
  structureRendering :: State -> H.ComponentHTML Query
  structureRendering state = case state.structure of
    Nothing -> HH.p_ [ HH.text ("Nothing") ]
    Just block' ->
      HH.div
          [ HE.onDragEnd (HE.input_ DragEnd)
          , classes ["structureView"]
          , HE.onDoubleClick (HE.input_ Join)
          ]
        [ structureToDiv state (BlockView [] block') ]
  
  structureToDiv :: State -> BlockView -> H.ComponentHTML Query
  structureToDiv _ (InputBlock loc typ) =
    HH.div  [ classes ["input", show typ]
            , HE.onDragOver (HE.input DragOver)
            , HE.onDrop (HE.input_ (Drop loc))
            ] []
  structureToDiv state blockView@(BlockView loc block'@(Block' block@(Block _ _ out _) connected)) =
    HH.div  [ HP.draggable true
            , classes ["block", show out, "void" `if_` null connected, "dragged" `if_` (state.dragSource == Just loc)]
            , HE.onDragStart (HE.input (Drag loc))
            , style $ flexGrow $ getWidth $ Just block'
            ]
      (
        maybe [] preview evaluated
        <> [ HH.div [HE.onClick (HE.input_ (Remove loc)), classes ["block-text"]]
          (blockToBody loc block)
        ]
        <> [ HH.div [classes ["block-inputs"]] $ map (structureToDiv state) (getConnected blockView)] `if_` not (null connected)
      )
    where
      evaluated = evaluate block'
  
  preview :: Val -> Array (H.ComponentHTML Query)
  preview (ColourVal colour) = [ HH.div [ classes [ "Colour", "preview" ], style $ backgroundColor $ rgb colour ] []  ]
  preview (IntVal int) = [ HH.div [ classes [ "Int", "preview" ] ] [ HH.text $ show int ] ]
  
  blockToBody :: Array Int -> Block -> Array (H.ComponentHTML Query)
  blockToBody loc block@(Block name fn out ins) =
    (if null ins then createInput loc block else [])
    <> [ HH.text ((name `if_` not (null ins)) <> typeSignature block) ]
  
  createInput :: Array Int -> Block -> Array (H.ComponentHTML Query)
  createInput loc (Block _ fn typ _) =
    [ HH.input $
      [ HE.onClick (HE.input StopPropagation)
      , HE.onInput (HE.input $ ChangeVal loc)
      , HE.onBlur (HE.input $ ResetVal loc) 
      ] <> case typ of
            IntType ->
              [ HP.type_ InputNumber
              , HP.prop (HCore.PropName "size") 5
              , HP.step (Step $ toNumber 1)
              , HP.value (toString (fn []))
              ]
            ColourType ->
              [ HP.type_ InputColor
              , HP.value (toString (fn []))
              ]
    ]
  
  toString :: Val -> String
  toString (IntVal i) = show i
  toString (ColourVal c) = colourToHex c
  
  getConnected :: BlockView -> Array BlockView
  getConnected (InputBlock loc _) = []
  getConnected (BlockView loc (Block' _ [])) = []
  getConnected (BlockView loc (Block' (Block _ _ _ ins) connected)) =
    zipWith (maybeToBlockView loc) (0 .. length connected) $ zip ins connected
  
  maybeToBlockView :: Array Int -> Int -> (Tuple Type (Maybe Block')) -> BlockView
  maybeToBlockView loc i (Tuple typ Nothing) = InputBlock (loc `snoc` i) typ
  maybeToBlockView loc i (Tuple typ (Just block')) = BlockView (loc `snoc` i) block'
  
  if_ :: forall a . Monoid a => a -> Boolean -> a
  if_ x bool = if bool then x else mempty

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (dom :: DOM | eff))
  eval = case _ of
    Remove loc reply -> do
      state <- H.get
      let nextState = state { structure = removeBlock (toUnfoldable loc) state.structure }
      H.put nextState
      pure reply
    Drag loc ev reply -> do
      state <- H.get
      H.liftEff $ stopPropagation (dragEventToEvent ev)
      draggingInput <- H.liftEff $ isInput ev
      when (draggingInput) $ H.liftEff $ preventDefault (dragEventToEvent ev)
      H.liftEff $ setEffectAllowed "move" ev
      let nextState = state { dragSource = Just loc }
      H.put nextState
      H.raise SetDrag
      pure reply
    StopPropagation ev reply -> do
      H.liftEff $ stopPropagation (mouseEventToEvent ev)
      pure reply
    DragEnd reply -> do
      state <- H.get
      let nextState = state { dragSource = Nothing }
      H.raise StopDrag
      H.put nextState
      pure reply
    DragOver ev reply -> do
      H.liftEff $ preventDefault (dragEventToEvent ev)
      H.liftEff $ setDropEffect "move" ev
      pure reply
    DragOverIfBlank ev reply -> do
      state <- H.get
      when (isNothing state.structure) do
        H.liftEff $ preventDefault (dragEventToEvent ev)
        H.liftEff $ setDropEffect "move" ev
      pure reply
    Drop loc reply -> do
      state <- H.get
      H.put $ state {dragTarget = Just loc}
      H.raise $ HandleDrop
      pure reply
    DropIfBlank reply -> do
      state <- H.get
      when (isNothing state.structure) (H.raise HandleDrop)
      pure reply
    RemoveDragged reply -> do
      state <- H.get
      let structure' = do
            source <- toUnfoldable <$> state.dragSource
            removeBlock source state.structure
      H.put $ state {structure = structure', dragSource = Nothing}
      pure $ reply unit
    GetDragged reply -> do
      state <- H.get
      pure $ reply do
        source <- toUnfoldable <$> state.dragSource
        structure <- state.structure
        blockAt source structure
    DropBlock block' reply -> do
      state <- H.get
      let structure' = do
            structure <- state.structure
            let srcType = getRet block'
            dest <- toUnfoldable <$> state.dragTarget
            destType <- getType dest structure
            guard $ srcType == destType
            insertBlock dest block' $ Just structure
      
      H.put $ state {structure = structure' <|> state.structure <|> Just block', dragTarget = Nothing}
      pure $ reply $ isJust structure' || isNothing state.structure
    ChangeVal loc ev reply -> do
      state <- H.get
      string <- H.liftEff $ inputValue ev
      traceShowA string
      let structure' = do
            let locList = toUnfoldable loc
            valType <- getType locList =<< state.structure
            val <- parseBlock valType string
            insertBlock locList val state.structure
      H.put $ state {structure = structure'}
      pure reply
    ResetVal loc ev reply -> do
      state <- H.get
      traverse_ (H.liftEff <<< setValue (focusEventToEvent ev)) $ toString <$> (evaluate =<< blockAt (toUnfoldable loc) =<< state.structure)
      pure reply
    Join reply -> do
      state <- H.get
      let structure' = joinBlocks "newBlock" <$> state.structure
      H.put $ state {structure = structure'}
      pure reply
    IsEmpty reply -> do
      state <- H.get
      pure (reply (isJust state.structure))

rgb :: Colour -> Color.Color
rgb (Colour r g b) = Color.rgb r g b

parseBlock :: Type -> String -> Maybe Block'
parseBlock IntType str = valBlock' <$> fromString str
parseBlock ColourType str = do
  {after: rgb} <- Str.splitAt 1 str
  guard $ Str.length rgb == 6
  {before :r', after: gb} <- Str.splitAt 2 rgb
  {before :g', after: b'} <- Str.splitAt 2 gb
  r <- fromStringAs hexadecimal r'
  g <- fromStringAs hexadecimal g'
  b <- fromStringAs hexadecimal b'
  pure $ valBlock' $ Colour r g b