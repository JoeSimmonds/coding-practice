blinker = [(2,2), (2,1), (2, 3)]

getNeighbours :: (Integer, Integer) -> [(Integer, Integer)]
getNeighbours (x, y) = [
    (x+1, y+0),
    (x-1, y+0),
    (x+1, y+1),
    (x+0, y+1),
    (x-1, y+1),
    (x+1, y-1),
    (x+0, y-1),
    (x-1, y-1)]

findAndIncrement :: [(Integer, Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer, Integer)]
findAndIncrement [] (a, b) = [(a, b, 1)]
findAndIncrement ((a,b,c):tail) (d, e)
    | a==d && b==e  = (a, b, c+1) : tail
findAndIncrement (head:tail) cell = head : findAndIncrement tail cell

findAllAndIncrement :: [(Integer, Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer, Integer)]
findAllAndIncrement cur [] = cur
findAllAndIncrement cur (head:tail) = findAllAndIncrement (findAndIncrement cur head) tail

buildNeighbourCounts :: [(Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
buildNeighbourCounts [] cur = cur
buildNeighbourCounts (head:tail) cur = buildNeighbourCounts tail (findAllAndIncrement cur (getNeighbours head))

isAlive :: [(Integer, Integer)] -> (Integer, Integer) -> Bool
isAlive [] _ = False
isAlive ((a, b):tail) (c, d)
    | a==c && b==d = True
    | otherwise = isAlive tail (c, d)

-- 1 Any live cell with fewer than two live neighbours dies, as if by underpopulation.
-- 2 Any live cell with two or three live neighbours lives on to the next generation.
-- 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
-- 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
singleCellAliveInNextgeneration :: [(Integer, Integer)] -> (Integer, Integer, Integer) -> Bool
singleCellAliveInNextgeneration worldState (x, y, c)
    | not (isAlive worldState (x, y)) && c == 3 = True
    | isAlive worldState (x, y) && (c==2 || c==3) = True
    | otherwise = False

extractCoords :: (Integer, Integer, Integer) -> (Integer, Integer)
extractCoords (x, y, c) = (x, y)

buildNewWorldState :: [(Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
buildNewWorldState priorState neighbourcounts = map extractCoords (filter (singleCellAliveInNextgeneration priorState) neighbourcounts)

step :: [(Integer, Integer)] -> [(Integer, Integer)]
step priorState = buildNewWorldState priorState (buildNeighbourCounts priorState [])


main = do 
    putStrLn (show (findAndIncrement [] (1,2)))
    putStrLn (show (findAndIncrement [(1,2,10), (3,4,5)] (1, 2)))    
    putStrLn (show (findAndIncrement [(1,2,10), (3,4,5)] (3, 4)))
    putStrLn (show (findAndIncrement [(1,2,10), (3,4,5)] (6, 4)))   
    putStrLn (show (findAllAndIncrement [(1,2,10), (3,4,5)] []))
    putStrLn (show (findAllAndIncrement [(1,2,10), (3,4,5)] [(1,2)]))
    putStrLn (show (findAllAndIncrement [(1,2,10), (3,4,5)] [(1,2), (6,7)]))
    putStrLn (show (buildNeighbourCounts [] []))
    putStrLn (show (buildNeighbourCounts [(1,5)] []))
    putStrLn (show (buildNeighbourCounts [(1,5), (2, 5)] []))
    putStrLn (show (buildNewWorldState [] []))
    putStrLn (show (isAlive [] (1,2)))
    putStrLn (show (isAlive [(1,2), (5,4)] (3, 5)))
    putStrLn (show (isAlive [(1,2), (5,4)] (5, 4)))
    putStrLn (show (buildNewWorldState [] [(1,1,3)])) -- rule 4
    putStrLn (show (buildNewWorldState [(1, 1)] [(1,1,1)])) -- rule 1
    putStrLn (show (buildNewWorldState [(1, 1)] [(1,1,2)])) -- rule 2
    putStrLn (show (buildNewWorldState [(1, 1)] [(1,1,3)])) -- rule 2
    putStrLn (show (buildNewWorldState [(1, 1)] [(1,1,4)])) -- rule 3
    putStrLn (show (step blinker))
    putStrLn (show (step (step blinker)))
    putStrLn (show (step (step (step blinker))))
    putStrLn (show (step (step (step (step blinker)))))
    putStrLn (show (step (step (step (step (step blinker))))))
    putStrLn (show (step (step (step (step (step (step blinker)))))))

    



