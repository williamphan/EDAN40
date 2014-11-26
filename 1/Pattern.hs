module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] [] = [] -- Empty lists
substitute _ [] _ = [] -- Stop when list is empty
substitute x (y:ys) zs	
 | x == y	= zs ++ (substitute x ys zs) -- x == y // y == x?
 | otherwise 	= y: (substitute x ys zs)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just [] --Nothing  -- []?
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (x:xs)
 | wc == p = orElse (singleWildcardMatch (p:ps) (x:xs)) (longerWildcardMatch (p:ps) (x:xs)) -- Found wildcard
 | p == x = match wc ps xs -- Continue to match
 | otherwise = Nothing 



-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
 -- | match wc ps xs /= Nothing = Just [x] 
 -- | otherwise = Nothing

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)




-- Test cases --------------------


testPattern =  "a=*;"
testSubstitutions = "32"  
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f xs (ys, zs) = mmap (substitute wc zs . f) (match wc ys xs)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f xs ys = foldr1 orElse $ map (transformationApply wc f ys) xs

