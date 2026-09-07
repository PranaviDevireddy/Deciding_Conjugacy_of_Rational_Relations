
data E_item = KS (String,String) 

data Stack_item = T (String,String) | Exp E_item | WS (String,String)

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

common_witness [WS w1, T (u,v)] = if iswit (fst w1) (u,v) || iswit (fst w1) (v,u)
                                  then Just (fst w1)
                                  else Nothing

common_witness (WS w1 : T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs ) = 
        let prim_expr = primitive_root 1 (u2,v2)
            redux = (u3++u1, v3++v1)
            prim_redux = primitive_root 1 redux
            prim_natural = primitive_root 1 (u1++u3,v1++v3)
        in
        if prim_redux == prim_expr then 
            if w1 == prim_redux then common_witness (WS prim_natural : T (u1++u3,v1++v3) : xs) 
            else Nothing
        else   
            case (witness (u2,v2)) of
                Nothing -> Nothing
                Just x  ->  let w_new = (u1 ++ x,v1) 
                            in
                            if (iswit x redux) then common_witness (WS w_new : T (u1++u3,v1++v3) : xs)
                            else Nothing
                        


common_witness (T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs ) = 
    let prim_expr = primitive_root 1 (u2,v2)
        redux = (u3++u1, v3++v1)
        prim_redux = primitive_root 1 redux
    in
    if prim_redux == prim_expr then common_witness ( WS prim_redux : T (u1++u3,v1++v3) : xs)
    else   
        case (witness (u2,v2)) of
            Nothing -> Nothing
            Just x -> if iswit x redux then 
                common_witness (WS (x, drop (length x) u1) : T redux : xs)
                else Nothing


main :: IO()
main = do 
    let eps = ""
    let stack = [T (eps,eps), Exp (KS (eps,eps)), T (eps,eps)]
    print (common_witness stack)
