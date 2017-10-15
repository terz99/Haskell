bin :: Int -> [Int]
bin x = tailBin x []
    where
        tailBin 0 li = li
        tailBin x li = tailBin (div x 2)((mod x 2) : li)
