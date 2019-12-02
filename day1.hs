
calculateModuleFuel :: Integer -> Integer
calculateModuleFuel mass = (div mass 3) - 2

mainPart1 :: IO ()
mainPart1 = do
    input <- readFile "input/day1.txt"
    let modules = map read (lines input)
    print . sum . map calculateModuleFuel $ modules

mainPart2 :: IO ()
mainPart2 = do
    input <- readFile "input/day1.txt"
    let modules = map read (lines input) 
    print . sum . map (sum . drop 1 . takeWhile (> 0) . iterate calculateModuleFuel) $ modules