type Peg = Char
type Move = [(Peg,Peg)]

hanoi :: Int -> Peg -> Peg -> Peg -> Move
hanoi 0 _ _ _ = []
hanoi 1 source endpoint _ = [(source,endpoint)]
hanoi n source endpoint auxiliary = hanoi (n-1) source auxiliary endpoint ++ hanoi 1 source endpoint auxiliary ++ hanoi (n-1) auxiliary endpoint source

main :: IO()
main = do
    n <- getLine
    let num = read n :: Int
    let result = hanoi num 'a' 'b' 'c'
    print result