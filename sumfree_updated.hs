
data E_item = KS (String,String) 

data Stack_item = T (String,String) | Exp E_item | WS (String,String) | WSU String

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


selective_repeat :: Int -> String -> String
selective_repeat 0 _ = ""
selective_repeat n x = x ++ (selective_repeat (n-1) x)


primitive_root :: Int -> (String, String) -> (String, String)
primitive_root n (u, v)
  | n >= length u = (u, v)  
  | otherwise =
      let x = selective_repeat (length u `div` n) (take n u)
          y = selective_repeat (length v `div` n) (take n v)
      in if x == u && y == v
         then (take n u, take n v)
         else primitive_root (n + 1) (u, v)


iswit :: String -> (String,String) -> Bool
iswit z (x,y) = x++z == z++y

witness :: (String,String) -> Maybe String
witness ((u,v)) = 
        let prim_root = primitive_root 1 (u,v)
        in
        case (cut prim_root) of 
            Nothing -> Nothing
            Just (x,y) -> Just x


common_witness :: [Stack_item] -> Maybe String

common_witness [WS w1, T (u,v)] = case witness w1 of
                                    Nothing -> Nothing
                                    Just z  -> if iswit z (u,v) || iswit z (v,u)
                                            then Just z
                                            else Nothing

common_witness [WSU x, T (u,v)] = if iswit x (u,v) || iswit x (v,u)
                                        then Just x
                                        else Nothing

common_witness (WSU x : T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs) =
    let prim_expr  = primitive_root 1 (u2,v2)
        redux      = (u3++u1, v3++v1)
        prim_redux = primitive_root 1 redux
    in
    if prim_redux == prim_expr then
        if iswit x prim_redux then common_witness (WSU x : T redux : xs)
        else Nothing
    else
        case witness (u2,v2) of
            Nothing -> Nothing
            Just y  ->
                if x == y && iswit x redux then common_witness (WSU x : T redux : xs)
                else Nothing


common_witness (WS w1 : T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs ) = 
        let prim_expr = primitive_root 1 (u2,v2)
            redux = (u3++u1, v3++v1)
            prim_redux = primitive_root 1 redux
            
        in
        if prim_redux == prim_expr then 
            if w1 == prim_redux then common_witness (WS prim_redux : T redux : xs) 
            else Nothing
        else   
            case (witness (u2,v2)) of
                Nothing -> Nothing
                Just x  ->  let w_new_bool = iswit x w1
                            in
                            if (iswit x redux && w_new_bool) then common_witness (WSU x : T redux : xs)
                            else Nothing
                        


common_witness (T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs ) = 
    let prim_expr = primitive_root 1 (u2,v2)
        redux = (u3++u1, v3++v1)
        prim_redux = primitive_root 1 redux
    in
    if prim_redux == prim_expr then common_witness ( WS prim_redux : T redux : xs)
    else   
        case (witness (u2,v2)) of
            Nothing -> Nothing
            Just x -> if iswit x redux then 
                common_witness (WSU x : T redux : xs)
                else Nothing


main :: IO()
main = do 
    let eps = ""
    let stack = [T ("ab","b"), Exp (KS ("bab","abb")), T ("b","ab")]
    print (common_witness stack)
