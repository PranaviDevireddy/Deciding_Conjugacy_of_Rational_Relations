data List_item = Tup (String,String) | K   (String,String)
    deriving Show

data Stack_item = Part List_item | Res (String,String)
    deriving Show


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

cut :: (String,String) -> Maybe (String,String)
cut (x,y) =  if length x /= length y then Nothing 
            else checkcut x y 0


checkcut :: String -> String -> Int -> Maybe (String,String)
checkcut x y i =
    if i > length y
    then Nothing
    else
        let before = take i y
            after  = drop i y
        in
            if after ++ before == x
            then Just (after,before)
            else checkcut x y (i + 1)


witness :: (String,String) -> String
witness (u,v) = case cut (u,v) of
        Just (a,b) -> a
        Nothing    -> ""


iswit :: String -> (String,String) -> Bool
iswit z (x,y) = if x ++ z == z ++ y then True else False


common_witness :: [Stack_item] -> IO Bool
common_witness [Res (x,y)] = do
    putStrLn x
    return True

common_witness (Part (Tup (x0,y0)) : Part (K (u,v)) : Part (Tup (x1,y1)) : xs ) =
    let x = x1 ++ x0
        y = y1 ++ y0
        inner_witness = witness (primitive_root (length u) (u,v))
    in
        if iswit inner_witness (x,y)
        then common_witness (Res (x,y) : xs)
        else do
            putStrLn "False"
            return False

main :: IO ()
main = do
    let stack = [Part (Tup ("b","a")) , Part (K ("ab","ba")), Part (Tup ("a","b"))]
    let w  = witness ("ab","ba")
    print w
    result <- common_witness stack
    print result