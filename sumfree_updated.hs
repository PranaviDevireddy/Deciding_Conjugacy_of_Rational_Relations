
data E_item = KS (String,String)
data Stack_item = T (String,String) | Exp E_item | Wit String

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

witness :: E_item -> Maybe String
witness (KS (u,v)) = case (cut (u,v)) of 
                        Nothing -> Nothing
                        Just (x,y) -> Just x

common_witness :: [Stack_item] -> Maybe String

common_witness [Wit z] = Just z
common_witness [Wit z, T (u,v)] = if iswit z (u,v) then Just z else Nothing
common_witness (Wit z : T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs) = 
    case (witness (Exp (KS (u2,v2)))) of
        Nothing -> Nothing
        Just x  -> 
            if ( x /= z ) then Nothing
            else 
                let (p_u1,p_v1) = primitive_root 1 (u1,v1) 
                    (p_u2,p_v2) = primitive_root 1 (u3,v3)
                    redux_u = p_u2 ++ p_u1
                    redux_v = p_v2 ++ p_v1
                in 
                if (iswit x (redux_u,redux_v)) then 
                    common_witness ( Wit z : T (redux_u,redux_v) : xs)
                else Nothing

common_witness ( T (u1,v1) : Exp (KS (u2,v2)) : T (u3,v3) : xs ) = 
    case (witness (Exp (KS (u2,v2)))) of
        Nothing -> Nothing
        Just x  -> 
            let (p_u1,p_v1) = primitive_root 1 (u1,v1) 
                (p_u2,p_v2) = primitive_root 1 (u3,v3)
                redux_u = p_u2 ++ p_u1
                redux_v = p_v2 ++ p_v1
            in 
            if (iswit x (redux_u,redux_v)) then 
                common_witness ( Wit x : T (redux_u,redux_v) : xs)
            else Nothing
                
main :: IO()
main = do 
    let stack = [T ("ab","ba"), Exp (KS ("u","v")), T ("ba","ab")]
    print (common_witness stack)
