selective_repeat :: Int -> String -> String
selective_repeat 0 x = ""
selective_repeat 1 x = x
selective_repeat n x =
    x ++ selective_repeat (n-1) x


primitive_root :: Int -> (String,String) -> (String,String)
primitive_root 0 (u,v) = (u,v)
primitive_root n (u,v) =
    let x = selective_repeat (length u `div` n) (take n u)
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