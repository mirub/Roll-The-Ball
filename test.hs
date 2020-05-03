import Data.Array as A

printArr :: (A.Array (Int, Int) Int) -> [Int]
printArr ana = [ana A.! (x, y) | x <- [0..1], y <- [0..2]]

testArray = array ((0, 0), (1, 1)) [((0, 0), 1),
                                      ((0, 1), 2),
                                      ((0, 2), 5)
                                      ((1, 0), 3),
                                      ((1, 1), 4)
                                      ((1, 2), 6)]
