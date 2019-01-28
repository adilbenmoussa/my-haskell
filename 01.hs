-- Lists

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallerOrEqualTo100  x = if x > 100 then x else x * 2
doubleSmallerOrEqualTo100PlusOne  x = (if x > 100 then x else x * 2) + 1

boomBang xs = [if x <= 10 then "BOOM!" else "BAM!!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUpperCase' st = [c | c <- st, c `elem` ['A' .. 'Z']]