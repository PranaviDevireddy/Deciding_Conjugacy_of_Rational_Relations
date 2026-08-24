

parsing :: String -> (String,[String])
parsing line = 
    let parts = words line
        lhs = head parts
        rhs = drop 2 parts
    in (lhs,rhs)


data Tree = Node String Tree Tree | Tuple (String,String)
    deriving Show


stack = ["S"]

get_rule :: String -> [(String,[String])] -> [String]

get_rule symbol [] = []
get_rule symbol ((lhs,rhs):xs) =
    if lhs == symbol
    then rhs
    else get_rule symbol xs


create_tree :: String -> [(String,[String])] -> Tree
create_tree symbol grammar = 
    let rhs = get_rule symbol grammar
    in 
        if (head rhs == "C" || head rhs == "U" || head rhs == "KS")
        then Node (head rhs)
            (create_tree (rhs !! 1) grammar)
            (create_tree (rhs !! 2) grammar)
        else
            let value = head rhs
                x = [value !! 1]
                y = [value !! 3]
            in Tuple (x,y)


data Item = T Tree | Result (String,String) | Op String
    deriving Show


process_stack :: [Item] -> (String,String)

process_stack [Result x] = x
process_stack (T (Tuple (x,y)) : xs) = process_stack (Result (x,y) : xs)
process_stack (T (Node "C" left right) : xs) = process_stack (T left : T right : Op "C" : xs)
process_stack (Result (x1,y1) : T right : Op "C" : xs) = process_stack (T right : Result (x1,y1) : Op "C" : xs)
process_stack (Result (x1,y1) : Result (x2,y2) : Op "C" : xs) = process_stack (Result (x1 ++ x2, y1 ++ y2) : xs)

find_cut :: (String,String) -> Maybe (String,String)

find_cut (x,y) =
    if length x /= length y then Nothing
    else check x y 0


check :: String -> String -> Int -> Maybe (String,String)

check x y i =
    if i > length x
    then Nothing
    else
        let a = take i x
            b = drop i x
        in if b ++ a == y
           then Just (a,b)
           else check x y (i + 1)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
    let grammar = map parsing ls
    let tree = create_tree "S" grammar

    let stack = [T tree]

    let pair = process_stack stack
    let cut = find_cut pair

    putStrLn ""
    print tree
    putStrLn ""
    putStrLn ""
    print cut
   