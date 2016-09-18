module Hanoi where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to temp = [(from, to)]
hanoi n from to temp = (hanoi (n-1) from temp to)
                        ++ [(from,to)]
                        ++ (hanoi (n-1) temp to from)

hanoi4peg :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4peg 1 from to temp1 temp2 = [(from, to)]
hanoi4peg 2 from to temp1 temp2 = [(from, temp1)] ++ [(from, to)] ++ [(temp1, to)]
hanoi4peg 3 from to temp1 temp2 = [(from, temp1)] ++ [(from, temp2)] ++ [(from, to)] ++ [(from, temp2)] ++ [(from, temp1)]
hanoi4peg n from to temp1 temp2 = (hanoi4peg halfNFl from temp1 to temp2) ++ (hanoi4peg halfNCl from temp2 to temp1)
