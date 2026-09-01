

data Tree = Node String (Maybe Tree) (Maybe Tree) | Tuple (String,String)
    deriving Show


data Item = T Tree | Result (String,String) | Op String
    deriving Show



parsing :: String -> (String,[String])
parsing line = 
    let parts = words line
        lhs = head parts
        rhs = drop 2 parts
    in (lhs,rhs)

stack = ["S"]


get_rule :: String -> [(String,[String])] -> [String]
get_rule symbol [] = []
get_rule symbol ((lhs,rhs):xs) =
    if lhs == symbol
    then rhs
    else get_rule symbol xs


split :: String -> (String,String)
split s =
    let x = takeWhile (/= ',') s
        y = tail (dropWhile (/= ',') s)
    in (x,y)


create_tree :: String -> [(String,[String])] -> Tree
create_tree symbol grammar =
    let rhs = get_rule symbol grammar
    in
        if head rhs == "C" || head rhs == "U"
        then
            Node (head rhs)
                 (Just (create_tree (rhs !! 1) grammar))
                 (Just (create_tree (rhs !! 2) grammar))

        else if head rhs == "KS"
        then
            Node (head rhs)
                 (Just (create_tree (rhs !! 1) grammar))
                 Nothing
        else
            let value = head rhs
                inside = init (tail value)
                parts = split inside
            in
                Tuple (fst parts, snd parts)


find_cut :: (String,String) -> Maybe (String,String)
find_cut (x,y) =
    if length x /= length y
    then Nothing
    else check x y 0


check :: String -> String -> Int -> Maybe (String,String)
check x y i =
    if i > length y
    then Nothing
    else
        let before = take i y
            after  = drop i y
        in
            if after ++ before == x
            then Just (after,before)
            else check x y (i + 1)
            

process_stack :: [Item] -> IO Bool

process_stack [Result (x,y)] =
    case find_cut (x,y) of
        Just (a,b) -> do
            putStrLn ("Cut: (" ++ a ++ "," ++ b ++ ")")
            putStrLn "conjugate"
            return True

        Nothing -> do
            putStrLn "not conjugate"
            return False

process_stack (T (Tuple (x,y)) : xs) = process_stack (Result (x,y) : xs)
process_stack (T (Node "C" (Just left) (Just right)) : xs) = process_stack (T left : T right : Op "C" : xs)
process_stack (Result (x1,y1) : T right : Op "C" : xs) = process_stack (T right : Result (x1,y1) : Op "C" : xs)
process_stack (Result (x1,y1) : Result (x2,y2) : Op "C" : xs) = process_stack (Result (x2 ++ x1, y2 ++ y1) : xs)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let ls = lines content
    let grammar = map parsing ls
    let tree = create_tree "S" grammar
    print tree
    is_conjugate <- process_stack [T tree]
    print is_conjugate
   







---------------------------------------------------------------
{*
selective_repeat :: Int -> String -> String
selective_repeat 0 x = ""
selective_repeat n x = x ++ selective_repeat (n-1) x


primitive_root :: Int -> (String,String) -> (String,String)
primitive_root 0 (u,v) = (u,v)
primitive_root n (u,v) = let x = selective_repeat (length u `div` n) (take n u)
                            y = selective_repeat (length v `div` n) (take n v)
                            in
                                if x == u && y == v
                                then (take n u, take n v)
                                else primitive_root (n-1) (u,v)


main :: IO ()
main = do
    let p = selective_repeat 3 "abc"
    let (u,v) = ("abab","bcbc")
    let (x,y) = primitive_root (length u - 1) (u,v)
    putStrLn p
    putStrLn x
    putStrLn y
*}