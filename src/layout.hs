{-# LANGUAGE BangPatterns, OverloadedStrings#-}
module Layout where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.List (foldl', groupBy)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Dom
import CSS
import Style

data Dimensions = Dimensions { x       :: Float
                             , y       :: Float
                             , width   :: Float
                             , height  :: Float
                             , padding :: EdgeSize
                             , border  :: EdgeSize
                             , margin  :: EdgeSize }

data EdgeSize = EdgeSize { left   :: Float
                         , right  :: Float
                         , top    :: Float
                         , bottom :: Float }

type LayoutBox = NTree (Dimensions,BoxType)

type StyledElement = (NodeType,PropertyMap)

data BoxType = BlockNode StyledElement | InlineNode StyledElement | AnonymousBlock

emptyEdge = EdgeSize 0 0 0 0

defaultDim = Dimensions 0 0 0 0 emptyEdge emptyEdge emptyEdge

data Display = Inline | Block | DisplayNone
  deriving (Eq)

-- if name exists, return its specified value
value :: StyledNode -> T.Text -> Maybe Value
value (NTree node _) name = HM.lookup name (snd node)

-- look up the display value of a node
display :: StyledNode -> Display
display n = case value n "display" of
    Just (Keyword "block") -> Block
    Just (Keyword "none")  -> DisplayNone
    _ -> Inline

-- this lookup is different than the robinson one, it supports
-- an arbitrary number of possible keywords

-- return the specified value of the first property in ks to exist
-- or def if no properties match
lookup :: StyledNode -> [T.Text] -> Value -> Value
Lookup s ks def = maybe def (fromJust . value s) (find (isJust . value s) ks)

toPx :: Value -> Float
toPx :: (Length len Px) = len
toPx _                  = o

buildLayoutTree :: StyledNode -> Either T.Text LayoutBox
buildLayoutTree root = case display root of
    Block       -> Right $ addDim <$> blt root
    Inline      -> Right $ addDim <$> blt root
    DisplayNone -> Left "error: root node has display:none"
  where
    addDim x = (defaultDim,x)

    blt rt@(NTree nd cs) = NTree n ns
      where 
        (!n, !ns) = case display rt of
            Block  -> (BlockNode  nd, anonify ns')
            Inline -> (InlineNode nd, ns')
            -- won't ever hit DisplayNone, it's filtered out

        anonify = concatMap mergeInlines . groupBy (\x y -> isInline x && isInline y)

        mergeInlines x = if isInline $ head x then [NTree AnonymousBlock x] else x

        isInline (NTree InlineNode{} _) = True
        isInline _                      = False

        ns' = map blt $ filter ((/=DisplayNone) . display) cs0

-- walk a layout tree, setting the dimensions of each node
layout :: LayoutBox -> Dimensions -> Either T.Text LayoutBox
layout l@(NTree (_,box)_) contBlock = case box of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


layoutBlock dim root = calcWidth dim root >>=
                       calcPosition dim   >>=
                       layoutChildren     >>= -- you know what? this might leak
                       calcHeight 

calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = do
    style <- getStyledElem root
    let
      auto = Keyword "auto"
      zero = Length 0 Px
      w = fromMaybe auto $ value style "width"
      vals = map (\a -> lookup style a zero) [
                    ["margin-left"       , "margin"]
                  , ["margin-right"      , "margin"]
                  , ["border-left-width" , "border-width"]
                  , ["border-right-width", "border-width"]
                  , ["padding-left"      , "padding"]
                  , ["padding-right"     , "padding"] ]
      total = sum $ map toPx (w:vals)
      underflow = width contBlock - total

      ([ml'',mr''],vals') = splitAt 2 vals
      (w',ml',mr') = checkUnderflow w $ checkAutoMargins (ml'',mr'')

      checkAutoMargins (x,y)
          | w /= auto && total > width contBlock = (check x,check y)
          | otherwise = (x,y)
        where check a = if a == auto then zero else a

      checkUnderflow w (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
          (False,False,False) -> (w , mlf, Length (toPx mrt + underflow) Px)
          (False,False,True)  -> (w , mlf, Length underflow Px)
          (False,True,False)  -> (w , Length underflow Px    , mrt)
          (False,True,True)   -> (w , Length (underflow/2) Px, Length (underflow/2) Px)
          (True,_,_)          ->
              let l = if mlf == auto then zero else mlf
                  r = if mrt == auto then zero else mrt
               in if underflow >= 0  then (Length underflow Px,l,r)
                                     else (zero,l,Length (toPx r + underflow) Px)

      [w'',ml,mr,blw,brw,plf,prt] = map toPx (w':ml':mr':vals')


      updateDim d = let pad = padding d
                        mar = margin d
                        bor = border d
                     in d{ width = w''
                         , padding = pad{ left = plf, right = prt}
                         , border  = bor{ left = blw, right = brw}
                         , margin  = mar{ left = ml,  right = mr} }

    return $ NTree (updateDim dim,x) y


getStyledElem :: LayoutBox -> Either T.Text StyledNode
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> Right $ NTree s []
    InlineNode s   -> Right $ NTree s []
    AnonymousBlock -> Left "Error: attempted to access the nonexistant\
                           \ StyleNode of an AnonymousBlock"

calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = checkBounds <$> getStyledElem rt
  where
    checkBounds = updateDim . checkUnderflow . checkAutoMargins . computeVals


calcPosition :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcPosition contBlock root@(NTree (dim,a)b) = do
    style <- getStyledElem root

    let
      zero = Length 0 Px

      vals = map (toPx .  (\a -> lookup style a zero)) [
                    ["margin-top"         , "margin"]
                  , ["margin-bottom"      , "margin"]
                  , ["border-top-width"   , "border-width"]
                  , ["border-bottom-width", "border-width"]
                  , ["padding-top"        , "padding"]
                  , ["padding-bottom"     , "padding"] ]

      updateDim d [mt,mb,bt,bb,pt,pb] =
          let pad = padding d
              mar = margin d
              bor = border d
              x' = x contBlock
                 + left (margin d)
                 + left (border d)
                 + left (padding d)
              y' = y contBlock + height contBlock + pt + bt + mt
           in d{ x = x'
               , y = y'
               , padding = pad{ top = pt, bottom = pb }
               , border  = bor{ top = bt, bottom = bb }
               , margin  = mar{ top = mt, bottom = mb } }

    return $ NTree (updateDim dim vals,a) b


-- recursively lay out the children of a node
layoutChildren (NTree (dim,x) cs) = do
    (dim',cs') <- foldM foo (dim,[]) cs
    return $ NTree (dim',x) cs'

    where
        foo (d,acc) c@(NTree (cdim,_) _) = do
            c' <- layout c d
            return (d{height = height d + marginBoxHeight cdim}, acc ++ [c'])


-- compute the hight of a box
calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight root@(NTree (d,x)y) = do
    s <- getStyledElem root
    let d' = case value s "height" of
             Just (Length h Px)  -> d{height=h}
             Nothing             -> d
    return $ NTree (d',x) y


marginBoxHeight :: Dimensions -> Float
marginBoxHeight (Dimensions _ _ _ h p b m) = sum [ h, top p, bottom p
                                                 , top b, bottom b
                                                 , top m, bottom m ]
