tail2 :: [a] -> [a]
tail2 (x:xs) = xs

(+++) [] b = b
(+++) (a:as) b = a: (+++) as b

init2 [a] = []
init2 (a:as) = a : init2 as

reverse2 :: [a] -> [a]
reverse2 al = rev2 al []
rev2 [] b = b
rev2 (a:as) b = rev2 as (a:b)

break2 :: (a -> Bool) -> [a] -> ([a],[a])
break2 f a = ([b | b <- a, (f b)], [c | c <- a, not (f c)])

max3 :: Ord a => [a] -> a
max3 [a] = a
max3 (a:a1:as) = 
    if (a1 > a)
        then max3 (a1:as)
        else max3 (a:as)
