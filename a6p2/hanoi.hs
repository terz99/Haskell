-- This function returns a sequence of moves to solve the towers of hanoi problem
-- The function has one argument --> the number of disks
-- the 'hanoi' function calls another recursive function 'recHanoi' which recursively
-- solves the problem. The functions arguments are:
--     - n --> the number of disks
--     - a --> the begining rod of any state (starts with 1)
--     - b --> the ending rod of any state (starts with 3)
--     - c --> the auxilary rod of any state (starts with 2)
-- This means that this problem shows the sequence of moves of transfering n disks
-- from rod #1 to rod #3 with the help of an auxilary rod #2.
-- The problem is divided in subproblems. There are two subproblems:
--     - recHanoi (n-1) a c b --> move n-1 disks from rod #a to #c
--     - recHanoi (n-1) c b a --> move n-1 disks from rod #c to #a
-- Terminology: I will refer to rods #a, #b and #c to current state's #a, #b, #c
--              because as a result of a recursive algorithm the order of a, b and c
--              is not constant
-- Basically, first we move n-1 disks from rod current state's #a to #c until we get to one disk
-- and move it to current state's rod #a to #b and then return the previous n-1 disks
-- to current state's #c to #b (Since originally we start from #a and the goal is #b. We achieve that using #c)


hanoi :: Int -> [(Int, Int)]
hanoi n = recHanoi n 1 3 2
    where
        recHanoi 0 a b c = []
        recHanoi n a b c = recHanoi (n-1) a c b ++ [(a, b)] ++ recHanoi (n-1) c b a
