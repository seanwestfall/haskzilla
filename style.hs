module Style where

import Data.Maybe (mapMaybe)
import Data.List (sortBy,find)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Dom
import CSS

type PropertyMap = HM.HashMap T.Text Value

-- instead of building a tree with references to the DOM, we'll
-- just augment the DOM tree with PropertyMaps
type StyledNode = NTree (NodeType,PropertyMap)

instance Functor NTree where
    fmap f (NTree n ns) = NTree (f n) $ fmap (fmap f) ns

findAttr :: ElementData -> T.Text -> Maybe T.Text
findAttr (ElementData _ m) k = HM.lookup k m

findID :: ElementData -> Maybe T.Text
findID = flip findAttr "id"

classes :: ElementData -> HashSet T.Text
classes = maybe empty (fromList . T.split (==' ')) . flip findAttr "class"

-- traverse the DOM, attaching PropertyMaps to each Node to
-- create a styled tree
styleTree :: Node -> Stylesheet -> StyledNode
styleTree root stylesheet = fmap style root
  where
    style e@(Element e') = (e, specifiedValues e' stylesheet)
    style t@(Text _)     = (t, HM.empty)

-- Build a map of all the properties attached to an Element
specifiedValues :: ElementData -> Stylesheet -> PropertyMap
specifiedValues e s = HM.fromList $ concatMap expand rules
  where
    rules = sortBy (compare `on` fst) $ matchingRules e s
    expand (_,Rule _ ds) = map (\(Declaration n v) -> (n,v)) ds


type MatchedRule = (Specificity, Rule)

-- get all of the rules from a stylesheet that match the given element
matchingRules :: ElementData -> Stylesheet -> [MatchedRule]
matchingRules e (Stylesheet rules) = mapMaybe (matchRule e) rules

-- find the first rule that matches the given element
matchRule :: ElementData -> Rule -> Maybe MatchedRule
matchRule e r@(Rule selectors _) = do
    s <- find (matches e) selectors
    return (spec s, r)

-- check if a selector matches an element
matches :: ElementData -> Selector -> Bool
matches e sl@(Simple _ _ _) = matchSimple e sl

fn matches_simple_selector(elem: &ElementData, selector: &SimpleSelector) -> bool {
    // Check type selector
    if selector.tag_name.iter().any(|name| elem.tag_name != *name) {
        return false;
    }

    // Check ID selector
    if selector.id.iter().any(|id| elem.id() != Some(id)) {
        return false;
    }

    // Check class selectors
    let elem_classes = elem.classes();
    if selector.class.iter().any(|class| !elem_classes.contains(&class.as_slice())) {
        return false;
    }

    // We didn't find any non-matching selector components.
    return true;
}

-- matchSimple returns False if any selector field that exists
-- does not match the given element
matchSimple :: ElementData -> Selector -> Bool
matchSimple e@(ElementData nm _) (Simple n i c) = 
  let x = fmap (==nm) n
      y = if i == Nothing then Nothing else Just $ i == (findID e)
      z = if not $ null c then all (flip HS.member (classes e)) c else True
  in case (x,y,z) of
      (Nothing, Nothing, b3) -> b3
      (Nothing, Just b2, b3) -> b2 && b3
      (Just b1, Nothing, b3) -> b1 && b3
      (Just b1, Just b2, b3) -> b1 && b2 && b3

matchSimple e (Simple Nothing  Nothing  c) =  matchClasses e c
matchSimple e (Simple (Just n) Nothing  c) =  matchNames e n
                                           && matchClasses e c
matchSimple e (Simple Nothing (Just i)  c) =  matchId e i
                                           && matchClasses e c
matchSimple e (Simple (Just n) (Just i) c) =  matchNames e n
                                           && matchId e i
                                           && matchClasses e c

matchNames (ElementData nm _) n = n == nm

matchId e i = findID e == Just i

matchClasses e [] = True
matchClasses e c = all (flip HS.member (classes e)) c


