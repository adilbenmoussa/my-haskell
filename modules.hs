import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map 
import qualified Data.Set as Set 
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube 

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' f g = \x y -> f (g x) (g y)

encode :: Int -> String -> String
encode shift msg = 
    let ords = List.map ord msg
        shifted = map (+ shift) ords
    in  List.map chr shifted

decode :: Int -> String -> String
decode shift = encode $ negate shift

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs) = if k == key
                             then Just v
                             else findKey key xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing 
