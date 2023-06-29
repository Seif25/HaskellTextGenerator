--Team 2 project
import DataFile

--wordToken

isElement a [] = False
isElement a (x:xs)
					| a == x = True
					| otherwise = isElement a xs

wordToken :: String -> [String]
wordToken s = punct1 (words s)
punct1 [] = []
punct1 (x:xs) = if isElement (last x) punct then (init x:[(last x)]:punct1 xs)
											else x:punct1 xs

--wordTokenList
wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList s = wordToken (unlines s)

--uniqueBigrams
uniqueBigrams :: [String] -> [(String,String)]

uniqueBigrams (s:[]) = []
uniqueBigrams (s:sa:sb) = if(elem (s,sa) (uniqueBigrams (sa:sb))) then (uniqueBigrams (sa:sb))
																  else ((s,sa):uniqueBigrams (sa:sb))

--uniqueTrigrams
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams (sa:sb:[]) = []
uniqueTrigrams (sa:sb:sc:sx) = if(elem (sa,sb,sc) (uniqueTrigrams (sb:sc:sx))) then (uniqueTrigrams (sb:sc:sx))
																  else ((sa,sb,sc):uniqueTrigrams (sb:sc:sx))

--bigramsFreq
occurs (x,xs) [] = 0
occurs (x,xs) (s:sx) = (if (x,xs) == s then 1 else 0) + occurs (x,xs) sx

tup (s:[]) = []
tup (s:sa:sb) = (s,sa):tup(sa:sb)


bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq (sa:[]) = []
bigramsFreq (s:sa:sb) = helper (s:sa:sb) (s:sa:sb)

helper (sa:[]) s = []
helper (s:sa:sb) (sx:sy:sz) = if(elem (s,sa) (uniqueBigrams (sa:sb))) then helper (sa:sb) (sx:sy:sz)
																	  else ((s,sa),occurs (s,sa) (tup (sx:sy:sz))):helper (sa:sb) (sx:sy:sz)
																	  
--trigramsFreq
occurs2 (xa,xb,xc) [] = 0
occurs2 (xa,xb,xc) (s:sx) = (if (xa,xb,xc) == s then 1 else 0) + occurs2 (xa,xb,xc) sx

tup2 (s:sa:[]) = []
tup2 (s:sa:sb:sc) = (s,sa,sb):tup2(sa:sb:sc)

trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq (sa:sb:[]) = []
trigramsFreq (sa:sb:sc:sx) = helperT (sa:sb:sc:sx) (sa:sb:sc:sx)

helperT (sa:sb:[]) s = []
helperT (sa:sb:sc:sx) (se:sf:sg:sh) = if(elem (sa,sb,sc) (uniqueTrigrams (sb:sc:sx))) then helperT (sb:sc:sx) (se:sf:sg:sh)
																	  else ((sa,sb,sc),occurs2 (sa,sb,sc) (tup2 (se:sf:sg:sh))):helperT (sb:sc:sx) (se:sf:sg:sh)
																	  
--getFreq
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq a [] = 0
getFreq a ((x,y):xs) = if (a==x) then y
								 else getFreq a xs
								 
--generateOneProb
generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((a,b,c),x) [] = x
generateOneProb ((a,b,c),x) (((w,y),m):xs) = if((a,b)==(w,y)) then x/m
															  else generateOneProb ((a,b,c),x) xs
															  
--genProbPairs
genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] ys = []
genProbPairs (((a,b,c),x):xs) ys = ((a,b,c),generateOneProb ((a,b,c),x) ys):genProbPairs xs ys

--generateNextWord
generateNextWord :: (Ord a, Fractional a) => [String] ->[((String,String,String),a)] -> [Char]
helperG n [] = []
helperG (x:y:ys) (((a,b,c),t):xs) = if((x,y)==(a,b) && t>0.03) then   (c:(helperG (x:y:ys) xs))  
															  else   (helperG (x:y:ys) xs)

generateNextWord (x:y:ys) (((a,b,c),t):xs) = if(length (helperG (x:y:ys) (((a,b,c),t):xs)) > 0 ) 
											 then (helperG (x:y:ys) (((a,b,c),t):xs))!! (randomZeroToX (length (helperG (x:y:ys) (((a,b,c),t):xs)) -1))
											 else error "Sorry, it is not possible to infer from current database"
																								  
--generateText
generateText :: String -> Int -> String																								  
generateText n k = unwords (helping (wordToken n) k)
--helping :: [String] -> String
helping (x:y:xs) n = if(n+2>0) then (x:( helping (y:generateNextWord [x,y] (helps docs):xs) (n-1)) )
							   else []
							 
helps l = genProbPairs (trigramsFreq (wordTokenList l)) (bigramsFreq (wordTokenList l))

--Haskell Test
sentToken:: String -> [String]
sentToken [] = []
sentToken (s:sx) = if s == '.' then ".":(sentToken sx)
				 else if s == '!' then "!":(sentToken sx)
				 else if s == '?' then "?":(sentToken sx)
				 else (s:head (sentToken sx)):tail (sentToken sx)

