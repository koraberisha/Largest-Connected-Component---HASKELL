-- Define types to ensure the code is more readable
type Cell = (Int,Int)
type Cells = [(Cell,Int)]

-- Main entry point function that takes 2D list of ints and a colour value then returns the length of the largest component
nlcc :: [[Int]] -> Int -> Int
nlcc l v = length (map fst $ getMaxComponent [((y,x),e) | (t,y) <- zip l [0..], (e,x) <- zip t [0..]] v)

-- Function to calculate the largest component by retieving all components and returning the size of the largest
-- This function removes duplicate components and components with colour values that are undesired them calculates the largest component
getMaxComponent :: Cells -> Int -> Cells
getMaxComponent cells color = foldl (\max current -> if length current > length max then current else max) [] 
     $ map (foldl(\current x -> if snd x /= color || x `elem` current then current else current ++ [x]) []      
      . retrieveComponent cells) cells

-- Function to construct the component a specific cell is apart of
-- This function takes an input cell, moves up, down, left and right of the cell and constructs the component recursively.
retrieveComponent :: Cells -> (Cell,Int) -> Cells
retrieveComponent _ (_,-1) = []
retrieveComponent cells ((x,y), c) = let current = ((x,y), c)
                                         moveup = retrieveCell cells (x, y + (-1))
                                         movedown = retrieveCell cells (x, y + 1)
                                         moveleft = retrieveCell cells (x + (-1), y)
                                         moveright = retrieveCell cells (x + 1, y)
                                         newCells = deleteCell cells ((x,y), c)
                                         findNeighbors = getRelatedCell c
                                         makeComponent = retrieveComponent newCells
                                    in current : makeComponent (findNeighbors moveup) ++ makeComponent (findNeighbors movedown) ++ makeComponent (findNeighbors moveleft) ++ makeComponent (findNeighbors moveright)

-- Function to get any nearby cells that are related to the current cell. I.e. have the same colour value
getRelatedCell :: Int -> (Cell,Int) -> (Cell,Int)
getRelatedCell c ((ix,iy),ic) = if (ix >= 0 && ix < 6 && iy >= 0 && iy < 8) && ic == c then ((ix,iy),ic) else ((-1,-1),-1)

-- Function to delete a cell from the list of cells returning a new list of cells not containing the desired one to delete
deleteCell :: Cells -> (Cell,Int) -> Cells
deleteCell [] _ = []
deleteCell (((ix,iy),ic):xs) ((x,y),c) = if ix /= x || iy /= y then  ((ix,iy),ic) : deleteCell xs ((x,y),c) else deleteCell xs ((x,y),c)

-- Function to retrieve the values of a Cell. Mainly the index and colour value of the cell
retrieveCell :: Cells -> Cell -> (Cell,Int)
retrieveCell [] _ = ((-1,-1), -1)
retrieveCell (((x,y),c):xs) (ix,iy) = if x == ix && y == iy then ((x,y),c) else retrieveCell xs (ix,iy)