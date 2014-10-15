module Main (main) where

import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad
import System.IO
import Text.Regex.Posix

makeList :: a -> [a]
makeList x = [x]

bipartitions :: (a -> Bool) -> [a] -> ([a], [a])
bipartitions f = g [] []
	where g a b []     = (reverse a, reverse b)
	      g a b (x:xs) = if f x then g (x:a) b xs else g a (x:b) xs

save_init :: [a] -> [a]
save_init [] = []
save_init [x] = []
save_init (x1:x2:xs) = x1 : (save_init (x2:xs))

fold_list :: ([a] -> ([a], b)) -> [a] -> [b]
fold_list _ [] = []
fold_list f xs = let (u, v) = f xs in v : (fold_list f u)

insertClass :: Eq a => [[a]] -> (a, a) -> [[a]]
insertClass [] (a, b) = [[a, b]]
insertClass (x:xs) (a, b)
	| a `elem` x && b `elem` x = (x:xs)
	| a `elem` x               = h x b xs
	| b `elem` x               = h x a xs
	| otherwise                = x : (insertClass xs (a, b))
	where h z c []     = [c:z]
	      h z c (y:ys) = if c `elem` y then (union z y) : ys else y : (h z c ys)

data Box a = Box {top :: Float, left :: Float, bottom :: Float, right :: Float, content :: a}

height :: Box a -> Float
height box = bottom box - top box

convert :: [Tag String] -> [((Float, Float), [Box String])]
convert = map (\x -> ((getAttrib "width" $ head x, getAttrib "height" $ head x) ,map f $ getWords x)) . sections (isTagOpenName "page")
	where getAttrib name t = read $ fromAttrib name t
	      getText = innerText . takeWhile (not . isTagCloseName "word")
	      f (t:ts) = Box (getAttrib "ymin" t) (getAttrib "xmin" t) (getAttrib "ymax" t) (getAttrib "xmax" t) (getText ts)
	      getWords = sections (isTagOpenName "word") . takeWhile (not . isTagCloseName "page")

clusterInterval :: (Num a, Ord a) => (b -> (a,a)) -> (b -> a) -> [b] -> [[b]]
clusterInterval _ _ [] = []
clusterInterval f h b = (\x -> g [head x] (snd $ f $ head x) (h $ head x) (tail x))  $ sortBy (comparing $ fst . f) b
	where g c _ _ []     = [c]
	      g c d e (x:xs)
	      	| (snd $ f x) <= d               = g (x:c) d           (max e $ h x) xs
	      	| (fst $ f x) <= d               = g (x:c) (snd $ f x) (h x) xs
	      	| (fst $ f x) - d <= max e (h x) = g (x:c) (snd $ f x) (h x) xs
	      	| otherwise                      = c : (g [x] (snd $ f x) (h x) xs)

clusterVertically :: Float -> [Box a] -> [[Box a]]
clusterVertically s = clusterInterval (\x -> (top x, bottom x)) (\x -> s * height x)

clusterHorizontally :: Float -> [Box a] -> [[Box a]]
clusterHorizontally s = clusterInterval (\x -> (left x, right x)) (\x -> s * height x)

cluster :: Float -> Float -> [Box a] -> [[Box a]]
cluster sVert sHor []    = []
cluster sVert sHor boxes = let yboxes = concatMap (clusterHorizontally sHor) $ clusterVertically sVert boxes
                           in if 1 == length yboxes then yboxes else concatMap (cluster sVert sHor) yboxes

rebox :: [Box a] -> Box [a]
rebox [] = error "cannot create box out of nothing"
rebox x  = Box (minimum $ map top x) (minimum $ map left x) (maximum $ map bottom x) (maximum $ map right x) (map content x)

reboxBy :: ([a] -> b) -> [Box a] -> Box b
reboxBy _ [] = error "cannot create box out of nothing"
reboxBy f x  = Box (minimum $ map top x) (minimum $ map left x) (maximum $ map bottom x) (maximum $ map right x) (f $ map content x)

liftBox :: (a -> b) -> Box a -> Box b
liftBox f b = Box (top b) (left b) (bottom b) (right b) (f $ content b)

sortBoxTopLeft :: [Box a] -> [Box a]
sortBoxTopLeft = sortBy $ comparing $ \b -> (top b, left b)

mergePage :: [Box String] -> [[[Box [Box String]]]]
mergePage []    = []
mergePage boxes =
	let
		indentations                     = map (\b -> left b) boxes
		heights                          = map (\b -> bottom b - top b) boxes
		height b                         = bottom b - top b
		isTableCaption w                 = w =~ "^(Table|table|TABLE|Tabelle|TABELLE)  *[0-9][0-9]*([.,][0-9][0-9]*)*:"
		isFigureCaption w                = w =~ "^(FIGURE|Figure|figure|Abbildung|ABBILDUNG)  *[0-9][0-9]*([,.][0-9][0-9]*)*:"
		captions                         = map fst $ filter ((\w -> isTableCaption w || isFigureCaption w) . content .snd) $ zip [0..] boxes
		cleanBuskets _ []                = []
		cleanBuskets zs (x:xs)           = let ys = filter (\y -> all (\z -> fst y /= z && snd y /= z) zs) x
		                                   in ys : (cleanBuskets (zs ++ (map fst ys) ++ (map snd ys)) xs)
	in
		map (map (map (liftBox (sortBoxTopLeft . map (boxes !!))) . sortBoxTopLeft) . clusterHorizontally 0.0 . concat) $
		groupBy (\a b -> length a > 1 && length b > 1) $
		clusterVertically 0.0 $
		map (reboxBy concat) $
		cluster 0.0 0.0 $
		map (\xs -> Box (minimum $ map (top . (!!) boxes) xs) (minimum $ map (left . (!!) boxes) xs)
		                (maximum $ map (bottom . (!!) boxes) xs) (maximum $ map (right . (!!) boxes) xs) xs) $
		(\xs -> (xs ++) $ map makeList $ foldl (\\) [0..(length boxes - 1)] xs) $
		concatMap (foldl insertClass []) $
		cleanBuskets [] $
		map (filter $ \(i,j) ->
			let hi = heights !! i; hj = heights !! j
			in (min hi hj) / (max hi hj) > 0.9 && indentations !! i >= indentations !! j) $
		map (concatMap snd) $
		groupBy (\a b -> fst a * 1.1 >= fst b) $
		map (\xs -> (snd $ head xs, map fst xs)) $
		groupBy (\a b -> snd a == snd b) $
		sortBy (comparing $ snd) $
		filter (\((i, j), d) -> (heights !! i)*0.75 > d && (heights !! j) * 0.75 > d) $
		concatMap (\((i,b):xs) ->
			map (\(j, c) -> ((i, j), top c - bottom b)) $
			(\ys -> foldl (\zs y -> filter (\z -> (bottom $ snd y) > (top $ snd z)) zs) ys ys) $
			filter (\(_,c) -> left c <= right b && left b <= right c && bottom b <= top c) xs) $
		save_init $ tails $ sortBy (comparing $ top . snd) $ zip [0..] boxes

unwordHyphens :: [String] -> String
unwordHyphens []     = ""
unwordHyphens [l]    = l
unwordHyphens (l:ls) = (if l =~ "-$" then save_init l else l ++ " ") ++ (unwordHyphens ls)

main :: IO ()
main = do
	wordBoxes <- getContents >>= (return . convert . canonicalizeTags . parseTags)
	forM_ wordBoxes $ \ page -> do
		putStrLn ""
--		putStrLn "next page"
		let levels = mergePage $ map (reboxBy (intercalate " ") . sortBy (comparing left)) $ cluster 0.0 1.0 $ snd page
		forM_ levels $  \level -> do
--			putStrLn "next level"
			forM_ level $ \column -> do
				forM_ column $ \paragraph -> do
					putStrLn $ unwordHyphens $ map content $ content paragraph 
--			let paragraphs = map (unwordHyphens . map content . content) $ concat level
--			let regroupedChunks = map unwordHyphens $ groupBy (\w _ -> w =~ "-$") paragraphs
--			mapM_ (putStrLn . ("\t" ++)) regroupedChunks
