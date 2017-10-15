import Data.List

isAnagram :: String -> String -> Bool
isAnagram a b
    | sort a == sort b = True
    | otherwise = False

anagrams :: [String] -> [[String]]
anagrams l = tailAnagrams [] [] [] l
    where
        tailAnagrams f_list [] [] [] = f_list
        tailAnagrams f_list c_list l_list []
            | null c_list = tailAnagrams f_list c_list [] l_list
            | otherwise = tailAnagrams (c_list : f_list) [] [] l_list
        tailAnagrams f_list [] l_list (r:rs) = tailAnagrams f_list (r : []) l_list rs
        tailAnagrams f_list (c:cs) l_list (r:rs)
            | isAnagram c r = tailAnagrams f_list (r : curr_list) l_list rs
            | otherwise = tailAnagrams f_list curr_list (r : l_list) rs
            where
                curr_list = (c:cs)
