import UI.HSCurses.Curses
import Data.Word
import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Bool (Bool (True))
import Data.Maybe (Maybe(Nothing))

{-
- Code is written by Thomas and Melvin over the course of several weeks
- A lot of code has been written in pair programming sessions
-}

data PlayerDirection = DirUp | DirDown | DirLeft | DirRight
data Node = Node (Int,Int)
data Player = Player [Node]

main :: IO()
main = game
{-Thomas-}
game :: IO()
game = do
    initScr                                                         --Initiate screen
    initCurses 
    timeout 0 
    cBreak True 
    cursSet CursorInvisible
    echo False                                                      --Disable writing of keyboardInputs on screen
    wBorder stdScr (changeBorder defaultBorder)                     --Draw border with help of changeBorder
    refresh
    threadDelay 100
    
    keyMVar <- newEmptyMVar
    forkIO $ do
        getLatestKeyLoop keyMVar



    maybeKey <- tryTakeMVar keyMVar
    let p1 = Player [ Node (5, 5), Node (5, 4)]
    let p2 = Player [ Node (85, 9), Node (85, 10)]
    win <- gameLoop stdScr keyMVar maybeKey (DirDown, DirUp) (p1, p2)
    bounds <- scrSize
    refresh 
    mvWAddStr stdScr (fst bounds-2) ((snd bounds) `div` 2) ("Game Over!")
    refresh 
    threadDelay 5000000;
    endWin


{-Melvin-}
getLatestKeyLoop :: MVar Key -> IO()
getLatestKeyLoop keyMVar = do
    key <- getCh
    putMVar keyMVar key
    getLatestKeyLoop keyMVar

{-Thomas-}
--Draw # alongside border of window.  
changeBorder :: Border -> Border
changeBorder (Border '\0' '\0' '\0' '\0' '\0' '\0' '\0' '\0') = (Border '#' '#' '#' '#' '#' '#' '#' '#')

{-Thomas-}
--Writes character(s) on specified x och y cord of screen 
--The header of mvwaddstr on curses library function --->
--int mvwaddstr(WINDOW *win, int y, int x, char *str);
drawChar :: Window -> Char -> Int -> Int -> IO() 
drawChar stdScr char y x = mvWAddStr stdScr y x (char: [])  

---------------------------------------------------------------------------------------------------------------------
--This section checks collision and takes care of movement
{-Thomas-}
--Check if two players collide with each other. If there is 
--collision return true
playerCollide :: Node -> Node -> Bool
playerCollide (Node (a, b)) (Node (c, d)) | (a == c) && (b == d) = True  --collision detected
                            | otherwise = False
{-Thomas-}
--Check if head of player collides with body of player. 
--If there is collision return true
playerCollideAny :: Node -> Player -> Bool
playerCollideAny a (Player xs) = any (playerCollide a) xs 

{-Melvin-}
playerHead :: Player -> Node
playerHead (Player []) = Node (0,0)
playerHead (Player (h:t)) = h
{-Melvin-}
prependNode :: Node -> Player -> Player
prependNode newHead (Player oldNodes) = Player (newHead : oldNodes)

{-Melvin-}
--Check if there is collision between players and border of screen. If there is 
--collision return true. 
borderCollide :: Node -> (Int, Int) -> Bool
borderCollide (Node (x, y)) (rows, cols) = (x <= 0) || (y <= 0) || (x >= cols-1) || (y >= rows-1)

{-Thomas-}
--Detect any kind of collision with help of playerCollide, playerCollideAny and 
--borderCollide to check if arrowCord. of player collides. Used more with drawing
-- arrowHead of players. 
collision :: Node -> (Int, Int) -> (Player, Player) -> Bool
collision newHead bounds (player1, player2) = borderCollide newHead bounds || playerCollideAny newHead player1 || playerCollideAny newHead player2

{-Thomas-}
--Add a tuple of int to a player to create movement
addVector :: Node -> Node -> Node
addVector (Node (a, b)) (Node(c, d)) = Node (a + c, b + d)

{-Melvin-}
--Convert PlayDirections to mathematical coordinates
headChangeVector :: PlayerDirection -> Node
headChangeVector DirRight = Node(1, 0)
headChangeVector DirLeft = Node(-1, 0)
headChangeVector DirUp = Node(0, -1)
headChangeVector DirDown = Node(0, 1)

{-Melvin-}
--Decide the shape of arrow based on direction
headChangeChar :: PlayerDirection -> Char
headChangeChar DirRight = '>'
headChangeChar DirLeft = '<'
headChangeChar DirUp = '^'
headChangeChar DirDown = 'v'

--------------------------------------------------------------------------------------------------------------------------

{-Thomas-}
--Check if enteredKey leads to a current dir that is impossible
--to move player into. Ex. If player is moving up it can't go down
--immediately. Incase such instances occur, "Nothing" is returned to
--make sure there is no self-collision of player.   
checkOpposites :: Maybe(Key) -> PlayerDirection -> Maybe(Key)
checkOpposites (Just(KeyChar 'w')) DirDown = Nothing             --key for up, down
checkOpposites (Just(KeyChar 'a')) DirRight = Nothing            --key for left, right
checkOpposites (Just(KeyChar 's')) DirUp = Nothing               --key for down, up
checkOpposites (Just(KeyChar 'd')) DirLeft = Nothing             --key for right, left
checkOpposites (Just(KeyChar 'i')) DirDown = Nothing
checkOpposites (Just(KeyChar 'j')) DirRight = Nothing
checkOpposites (Just(KeyChar 'k')) DirUp = Nothing
checkOpposites (Just(KeyChar 'l')) DirLeft = Nothing
checkOpposites maybeKey _ = maybeKey                             --Otherwise enteredKey doesn't lead to self-collision

{-Both Melvin & Thomas have written parts-}
gameLoop :: Window -> MVar Key -> Maybe(Key) -> (PlayerDirection, PlayerDirection) -> (Player, Player) -> IO (Bool) 
gameLoop stdScr keyMVar maybeKey (p1OldDir, p2OldDir) (player1, player2) = do
        --Changes directions depending on contents of Key MVar
        if maybeKey == (Just(KeyChar 'q')) 
            then do
                putStrLn "QUIT"
                return False
            else do
                positionUpdate stdScr keyMVar (p1dir, p2dir) (player1, player2) 

    where 
        oppPlayer1 = checkOpposites maybeKey p1OldDir       --Check if direction leads to self-collision with checkOpposites
        p1dir = case oppPlayer1 of
            Just(KeyChar 'w') -> DirUp
            Just(KeyChar 'a') -> DirLeft
            Just(KeyChar 's') -> DirDown
            Just(KeyChar 'd') -> DirRight
            _ -> p1OldDir
    
        oppPlayer2 = checkOpposites maybeKey p2OldDir       --Check if direction leads to self-collision with checkOpposites
        p2dir = case oppPlayer2 of
            Just(KeyChar 'i') -> DirUp
            Just(KeyChar 'j') -> DirLeft
            Just(KeyChar 'k') -> DirDown
            Just(KeyChar 'l') -> DirRight
            _ -> p2OldDir




{-Melvin-}
-- Takes in a window, a point and the two players
-- The point might have an old arrow drawn on it
-- If the point collides with bounds or a player, do not change anything
-- If the point does not collide, write a space there to replace possible arrows
checkClearPoint :: Window -> (Player, Player) -> Node -> IO()
checkClearPoint stdScr players point = do
    bounds <- scrSize -- Get current screen bounds
    if collision point bounds players 
        then do
            -- Collides with bounds or player
            return ()
        else do
            -- Clear the point which might have an arrow
            drawChar stdScr ' ' (snd (nodeToTuple point)) (fst (nodeToTuple point))

{-Melvin-}
checkClearList :: Window -> [Node] -> (Player, Player) -> IO()
checkClearList stdScr nodes players = do
    -- For every point, attempt to clear any possible arrows
    mapM (checkClearPoint stdScr players) nodes
    return ()

{-Melvin-}
-- From the window, two current directions and the players,
-- (1) Clear any possible previous arrows 
-- (2) Draw the new arrows one tile ahead in the current direction
drawArrow :: Window -> (PlayerDirection, PlayerDirection) -> (Player, Player) -> IO()
drawArrow stdScr (p1dir, p2dir) (player1, player2) = do
    
    -- (1) Get the upcoming positions,
    -- (1) Coordinates of where to draw new arrows
    let p1NewHead = addVector (playerHead player1) (headChangeVector p1dir)
    let p2NewHead = addVector (playerHead player2) (headChangeVector p2dir)

    -- (1) Partial function applications
    -- (1) The resulting functions adds a vector to the specific player head
    let addToP1Head = addVector (playerHead player1)
    let addToP2Head = addVector (playerHead player2)

    -- (1) Lists of points where previous arrows could be
    -- (1) They are 4 long, one step in every direction from each head
    let p1Possible = map addToP1Head (map headChangeVector allDirs)
    let p2Possible = map addToP2Head (map headChangeVector allDirs)
    -- CAUTION: This code will cause graphical issues if the user were to turn on the last possible frame / the frame of movement 
    -- Since the previous arrow could be excluded from the list of possible coordinates
    -- The frame rate / movement rate is 2000 so the probability is tiny

    -- (1) Append the two lists of possible coordinates and players
    -- (1) Check and clear all possible previous arrows
    checkClearList stdScr (p1Possible ++ p2Possible) (player1, player2)


    -- (2) Get bounds for collission checking
    bounds <- scrSize
    if (collision p1NewHead bounds (player1, player2)) || (collision p2NewHead bounds (player1, player2))
        then do
            -- Collision with player or border, do nothing
            return ()

        else do
            -- No collision, draw the proper arrow at the correct coordinates
            drawChar stdScr (headChangeChar p1dir) (snd (nodeToTuple p1NewHead)) (fst (nodeToTuple p1NewHead))
            drawChar stdScr (headChangeChar p2dir) (snd (nodeToTuple p2NewHead)) (fst (nodeToTuple p2NewHead))
    where 
        allDirs = [DirUp, DirDown, DirLeft, DirRight]

    




{-Both Melvin & Thomas have written parts-}
-- Function for updating the arrow direction without moving the players
directionUpdate :: Int -> Window -> MVar Key-> Maybe(Key)-> (PlayerDirection, PlayerDirection) -> (PlayerDirection, PlayerDirection)-> (Player, Player) -> IO ((PlayerDirection, PlayerDirection))  --old direction, new direction
directionUpdate 0 _ _ _ _ directions _ = do
    -- Finished, return the current directions
    return directions
directionUpdate num stdScr keyMVar maybeKey (p1OldDir, p2OldDir) (p1newDir, p2NewDir) (player1, player2) = do
    -- Read current user input
    newMaybeKey <- tryTakeMVar keyMVar
    -- Only used in upcoming recursive calls

    -- Clear previous arrows & draw a new one
    drawArrow stdScr (p1dir, p2dir) (player1,player2)

    refresh
    threadDelay 150

    -- Recursive call, reducing number of remaining cycles by 1
    directionUpdate (num - 1) stdScr keyMVar newMaybeKey (p1OldDir, p2OldDir) (p1dir, p2dir) (player1, player2)
    where 
        oppPlayer1 = checkOpposites maybeKey p1OldDir                   --Check if direction leads to self-collision with checkOpposites
        p1dir = case oppPlayer1 of
            Just(KeyChar 'w') -> DirUp
            Just(KeyChar 'a') -> DirLeft
            Just(KeyChar 's') -> DirDown
            Just(KeyChar 'd') -> DirRight
            _ -> p1newDir
    
        oppPlayer2 = checkOpposites maybeKey p2OldDir                   --Check if direction leads to self-collision with checkOpposites
        p2dir = case oppPlayer2 of
            Just(KeyChar 'i') -> DirUp
            Just(KeyChar 'j') -> DirLeft
            Just(KeyChar 'k') -> DirDown
            Just(KeyChar 'l') -> DirRight
            _ -> p2NewDir

{-Melvin-}
nodeToTuple :: Node -> (Int, Int)
nodeToTuple (Node a) = a

{-Both Melvin & Thomas have written parts-}
positionUpdate :: Window -> MVar Key-> (PlayerDirection, PlayerDirection) -> (Player, Player) -> IO (Bool) 
positionUpdate stdScr keyMVar (p1dir, p2dir) (player1, player2) = do  
    let p1NewHead = addVector (playerHead player1) (headChangeVector p1dir)
    let p2NewHead = addVector (playerHead player2) (headChangeVector p2dir)
    bounds <- scrSize
    if collision p1NewHead bounds (player1, player2) 
        then do 
            putStrLn "Player 1 collision!"
            return False 
        else do
            if collision p2NewHead bounds (player1, player2) 
                then do 
                    putStrLn "Player 2 collision!"
                    return True 
                else do
                    drawChar stdScr '#' (snd (nodeToTuple p1NewHead)) (fst (nodeToTuple p1NewHead))
                    drawChar stdScr '#' (snd (nodeToTuple p2NewHead)) (fst (nodeToTuple p2NewHead))
                    --drawArrow stdScr (p1dir, p2dir) (p1NewHead:player1,p2NewHead:player2)

                    refresh
                    maybeKey <- tryTakeMVar keyMVar 
                    (p1NewDir, p2NewDir) <- directionUpdate 150 stdScr keyMVar maybeKey (p1dir, p2dir) (p1dir, p2dir) (prependNode p1NewHead player1,prependNode p2NewHead player2)
                    gameLoop stdScr keyMVar maybeKey (p1NewDir, p2NewDir) (prependNode p1NewHead player1, prependNode p2NewHead player2)
