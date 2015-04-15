-- | Main entry point to the application.
module Main where

-- 1. Make a function named 'myLast' that finds the last element of a list.
myLast :: [a] -> a
myLast = last

-- 2. Make a function named 'myButLast' that finds the second last element of a list.
myButLast :: [a] -> a
myButLast = head .tail . reverse

-- 3. Make a function named 'elementAt' that finds the element of a list at a specific 1-based index.
elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

-- 4.  Make a function named 'myLength' that finds the number of elements in a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5. Make a function named 'myReverse' that reverses a list.
myReverse :: [a] -> [a]
myReverse a = foldr (\x xs -> xs ++ [x]) [] a

-- 6. Make a function named 'isPalindrome' that finds whether a list is a palindrome (is the same read forward and backward).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 7. Make a function named 'flatten' that flattens a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8. Make a function named 'compress' removes consecutive duplicates of elements in a list wi(thout affecting the order.
compress :: Eq a => [a] -> [a]
compress [a] = [a]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = x: (compress xs)

-- 9. Make a function named 'pack' that packs consecutive duplicates and standalone elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack x =
    let
        pack' h [] = [[h]]
        pack' h (x:xs)
            | head x == h = h:x ++ xs
            | otherwise = [h]:xs
    in
        foldl(pack') [] x

-- | The main entry point.
main :: IO ()
main = do
{-
    putStrLn $ show (isPalindrome [1,2,3,1])
    putStrLn $ show (isPalindrome "madamimadam")
    putStrLn $ show (isPalindrome [1,2,4,8,16,8,4,2,1])

    putStrLn $ show (flatten (Elem 5))
    putStrLn $ show (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    putStrLn $ show (flatten (List []))

    putStrLn $ show (compress "aaaabccaadeeee")
    putStrLn $ show (compress [1,1,2,3,1,2,2,3])
-}

    putStrLn $ show (pack "aaaabccaadeeee")
    putStrLn $ show (pack [1,1,2,3,1,2,2,3])
{-
    putStrLn $ show ()
    putStrLn $ show ()
    putStrLn $ show ()
-}
