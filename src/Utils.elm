module Utils exposing (partitionByN)

import List


partitionByN : Int -> List a -> List (List a)
partitionByN n list =
    if List.isEmpty list then
        []
    else
        let
            catch =
                (List.take n list)
        in
            if n == (List.length catch) then
                [ catch ] ++ (partitionByN n (List.drop n list))
            else
                [ catch ]
