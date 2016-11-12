module Game where

import Prelude (class Show, class Eq, show, const, map, otherwise, ($), (<>),
(==), (<<<), pure, (&&), (||), (>), class Functor, (<$>), negate, Ordering, compare,
(-), (+), (>=), (<=), (<))
import Data.Foldable (sum, any, maximumBy, minimumBy, maximum, minimum)
import Data.Maybe (Maybe(..), isNothing, isJust, fromJust)
import Data.Array ((..), filter, (!!), length, concat, catMaybes, mapMaybe, zip)
import Data.Array.Partial (head, tail)
import Data.Tuple
import Partial.Unsafe (unsafePartial)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, text, span)
import Pux.Html as H --Couldn't get 'i' to reference the correct thing.
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)


-------------------------------------------------------------------------------
-- Data type definitions
-------------------------------------------------------------------------------
data Token = X | O
instance showToken :: Show Token where
  show X = "X"
  show O = "O" 
instance eqToken :: Eq Token where
  eq X X = true
  eq O O = true
  eq _ _ = false

-- | Get the other playable token.
not :: Token -> Token
not X = O
not O = X

data Result = XWins | OWins | Draw


-- "Diagonals" to check for 3 in a row.
diags :: Array (Array Int)
diags =  [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9],
          [1, 4, 7],
          [2, 5, 8],
          [3, 6, 9],
          [1, 5, 9],
          [3, 5, 7]]


type Square = Maybe Token
type Cell = Tuple Int Square
type Board = Array (Cell)
data Move = Move Int Token

-------------------------------------------------------------------------------
-- Pux Architecture definitions
-------------------------------------------------------------------------------

data Action = Play Int
            | Computer Int
            | ReportChoice Token
            | CheckWinner
            | ShowResult Result 
            | UpdateScore Result
            | Reset

-- An element of the board is modeled as a tuple of the position (Int) and the
-- Square it holds.
type State = { board :: Board
             , player :: Token
             , current :: Token
             , score :: { xWins :: Int, oWins :: Int, draws :: Int } 
             , gettingChoice :: Boolean
             , result :: Maybe Result 
           }

-- | Initialise the board as an array of nine empty Squares.
init :: State
init =  { board: map (\i -> Tuple i Nothing) (1..9) 
        , current: X
        , player: X
        , score: {xWins: 0, oWins: 0, draws: 0 } 
        , gettingChoice: true
        , result: Nothing
        }
reset :: State -> State
reset state = state { board = map (\i -> Tuple i Nothing) (1..9)
                    , current = X
                    , result = Nothing }

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------
update :: forall e. Action -> State -> EffModel State Action (e)
update (Play i) state = { state: newState
                        , effects: [
                                   if gameOver newState.board then
                                     pure $ ShowResult $ getResult newState.board
                                   else
                                     pure $ getComputerMove newState
                                   ]
                        }
  where newState = state { board = updateBoard i state.current state.board
                         , current = not state.current }

update (Computer i) state = { state: newState i                        
       , effects: [ pure CheckWinner ]
     }
       where newState i | i == 0    = state 
             | otherwise = state { board = updateBoard i state.current state.board
                                 , current = not state.current 
                               }

update (ReportChoice X) state = 
  noEffects $ state {player = X, gettingChoice = false} 
update (ReportChoice O) state =
  { state: state {player = O, gettingChoice = false } 
  , effects: [ pure $ getComputerMove state ] 
  }

update CheckWinner state = if gameOver state.board
                              then { state: state
                                   , effects: [ 
                                              pure $ ShowResult $ getResult state.board 
                                              ]
                                            }
                                            else noEffects state

update (ShowResult result) state = noEffects $ state { result = (Just result) }
update Reset state = 
  {state: reset state, 
   effects: [pure $ UpdateScore $ unsafePartial $ fromJust state.result]}

update (UpdateScore XWins) state = noEffects $ state {score = state.score {xWins = state.score.xWins + 1}}
update (UpdateScore OWins) state = noEffects $ state {score = state.score {oWins = state.score.oWins + 1}}
update (UpdateScore Draw) state = noEffects $ state {score = state.score {draws = state.score.draws + 1}}


-- | updateBoard takes a position and a Token, and returns a new Board with the
-- | updated Square.
updateBoard :: Int -> Token -> Board -> Board
-- I did it with a map as a safer way to change a value in the list. (All the
-- standard Array functions are unsafe and return a Maybe type.
updateBoard i t = map (updateSquare i t)
  where updateSquare i t (Tuple i' t') | i == i'   = Tuple i (Just t)
                                       | otherwise = Tuple i' t'

-------------------------------------------------------------------------------
-- View and auxiliary Html functions
-------------------------------------------------------------------------------

view :: State -> Html Action
view state = div [] [ 
                 chooserHtml state,
                 div [className "game"] 
                 [
                   boardHtml state,
                   scoreHtml state,
                   resultHtml state
                   ]
                   ]

chooserHtml :: State -> Html Action 
chooserHtml state = 
  if state.gettingChoice then
   div [className "chooser"] 
   [
     div [className "chooser-box"]
         [ div [] [text "Pick a side. Choose wisely. "],
           div [className "choice", onClick $ const (ReportChoice X)] [getFaIcon X],
           div [className "choice", onClick $ const (ReportChoice O)] [getFaIcon O]
         ]
     ] 
     else div [] []

boardHtml :: State -> Html Action 
boardHtml state = div [className "board"] $ 
  map (if isNothing state.result then squareHtml else unplayableSquareHtml) state.board

scoreHtml :: State -> Html Action
scoreHtml state = 
  div [className "score"] 
  [
    text $ "Score: ",
    getFaIcon X, text $ "  " <> show state.score.xWins <> ",  ",
    getFaIcon O,
    text $ "  " <> show state.score.oWins <> ", ",
    text $ "  Draw: " <> show state.score.draws
    ]

squareHtml :: Tuple Int Square -> Html Action
squareHtml (Tuple i Nothing) = div
  [className "square empty", onClick (const (Play i))]
  []
squareHtml (Tuple i (Just t)) = div
  [className $ "square " <> show t]
  [
    getFaIcon t
    ]

unplayableSquareHtml :: Tuple Int Square -> Html Action
unplayableSquareHtml (Tuple i Nothing) = div
  [className "square"]
  []
unplayableSquareHtml (Tuple i (Just t)) = div
  [className $ "square " <> show t]
  [
    getFaIcon t
    ]

resultHtml :: State -> Html Action
resultHtml state = if isNothing state.result
                      then div [] []
                      else div [className "result"] [
                        div [] [ winner state.result
                               , text " wins!"
                               ],
                        div [className "again", onClick (const Reset)] [text "Play again?"]
                               ]
                                 where winner (Just XWins) = getFaIcon X
                                       winner (Just OWins) = getFaIcon O
                                       winner (Just Draw)  = text "Nobody" 
                                       winner Nothing = text ""




getFaIcon :: forall a. Token -> Html a
getFaIcon X = H.i [className $ "X fa fa-times"] []
getFaIcon O = H.i [className $ "O fa fa-circle-o"] []

--------------------------------------------------------------------------------
-- Scoring and AI
--------------------------------------------------------------------------------
getCell :: Board -> Int -> Maybe Cell 
getCell board i = board!!i

cellDiags :: Board -> Array (Array Int) -> Array (Array Cell)
-- Some "diags" will potentially have less cells, if cells are not found in the
-- board. However, this shouldn't influence the scoring, so we don't care.
-- Reminder: mapMaybe filters out the "Nothing"s and returns a regular array.
cellDiags board dgs = map (mapMaybe $ getCell board) dgs

scoreFor :: Token -> Int -> Array Cell -> Boolean
scoreFor t n cells = length matches == n
  where 
    match :: Cell -> Boolean
    match (Tuple _ (Just t')) = t' == t
    match _ = false
    matches = filter match cells

scorePos :: Board -> Token -> Int -> Int 
scorePos board t i = length $ filter (eq (Tuple i (Just t))) board 
  where eq (Tuple _ Nothing) _ = false 
        eq _ (Tuple _ Nothing) = false 
        eq (Tuple i (Just t)) (Tuple j (Just t')) = (i == j && t == t' )

score :: Board -> Token -> Array Int -> Int
score board t is = sum $ map (scorePos board t) is 

filterForScore :: Board -> Token -> Int -> Array (Array Int) -> Array (Array Int)
filterForScore board t num dgs = filter (\diag -> score board t diag  == num) dgs

getResult :: Board -> Result
getResult board = if checkWin O board then OWins else 
                  if checkWin X board then XWins else Draw 

getComputerMove :: State -> Action
-- | Generate a list of possible moves and start a minimax on all of them. Then,
-- | get the highest scoring move.
getComputerMove state = doMove <<< fst $ bestMove state.current scoredMoves 
  where moves = getMoves state.current state.board
        newBoards = map (applyMove state.board) moves
        {-- scores = map (minimax 5 state.current) newBoards --}
        scores = map (abMinimax 5 (not state.current) (-100) 100) newBoards
        scoredMoves = zip moves scores

doMove :: Move -> Action
doMove (Move 0 _ ) = CheckWinner
doMove (Move i token) = Computer i

--------------------------------------------------------------------------------
-- Minimax implementation
--------------------------------------------------------------------------------
{-- minimax :: Int -> Token -> Board -> Int --}
{-- minimax depth token board --} 
{--   | gameOver board = evaluate board --}
{--   | depth == 0     = 0 --} 
{--   | otherwise = case token of --} 
{--      X -> maximum' $ map (minimax (depth - 1) O) newBoards --}
{--      O -> minimum' $ map (minimax (depth - 1) X) newBoards --}
{--       where --} 
{--         moves = getMoves (not token) board --}
{--         newBoards = map (applyMove board) moves --} 

--------------------------------------------------------------------------------
-- Alpha-beta pruning implementation
--------------------------------------------------------------------------------
-- Rather than map over the set of next moves, we have to do implement a way to
-- iterate over all moves, all the while passing along the values of alpha and
-- beta, as well as be able stop the "iteration" as soon as the break condition
-- is met.
abMinimax :: Int -> Token -> Int -> Int -> Board -> Int
abMinimax depth token alpha beta board
  | gameOver board = evaluate board
  | depth == 0     = 0
  | otherwise      = case token of
    X -> maximum' $ maxAbMap alpha beta (abMinimax (depth - 1) O) newBoards
    O -> minimum' $ minAbMap alpha beta (abMinimax (depth - 1) X) newBoards
      where 
        moves = getMoves token board
        newBoards = map (applyMove board) moves 

maxAbMap :: Int -> Int -> (Int -> Int -> Board -> Int) -> Array Board -> Array Int
maxAbMap alpha beta fun [] = []
maxAbMap alpha beta fun bs = if score >= beta 
                                then [beta]
                             else if score > alpha 
                                then [score] <> maxAbMap score beta fun rest
                             else    [score] <> maxAbMap alpha beta fun rest
  where score = unsafePartial $ fun alpha beta (head bs)
        rest = unsafePartial $ tail bs

minAbMap :: Int -> Int -> (Int -> Int -> Board -> Int) -> Array Board -> Array Int
minAbMap alpha beta fun [] = []
minAbMap alpha beta fun bs = if score <= alpha
                               then [alpha]
                             else if score < beta
                               then [score] <> minAbMap alpha score fun rest
                             else   [score] <> minAbMap alpha beta  fun rest
  where score = unsafePartial $ fun alpha beta (head bs)
        rest = unsafePartial $ tail bs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Safe definitions of maximum and minimum, where a Nothing is replaced by 
-- | a zero score.
maximum' :: Array Int -> Int
maximum' xs = 
  case max of
    Just m -> m
    Nothing -> 0
      where max = maximum xs

minimum' :: Array Int -> Int
minimum' xs = 
  case min of
    Just m -> m
    Nothing -> 0
      where min = minimum xs


isEmpty :: Cell -> Boolean
isEmpty (Tuple i sq) = isNothing sq

gameOver :: Board -> Boolean
gameOver board =  checkWin X board 
               || checkWin O board 
               || checkFull board
  where checkFull board = (length $ filter isEmpty board) == 0

checkWin :: Token -> Board -> Boolean
checkWin t board = (length $ filterForScore board t 3 diags) > 0

evaluate :: Board -> Int
evaluate board | checkWin X board = 10
               | checkWin O board = (-10)
               | otherwise = 0

applyMove :: Board -> Move -> Board
applyMove board (Move i t) = map (updateCell i t) board
  where updateCell i t (Tuple i' t') | i == i'   = Tuple i (Just t)
                                     | otherwise = Tuple i' t'

getMoves :: Token -> Board -> Array Move 
getMoves token board = map (makeMove token) $ filter isEmpty board
  where makeMove token (Tuple i _) = Move i token

bestMove :: Token -> Array (Tuple Move Int) -> Tuple Move Int
bestMove X moves = case (maximumBy comp moves) of
                        Just m -> m
                        Nothing -> Tuple (Move 0 X) 0
bestMove O moves = case (minimumBy comp moves) of
                        Just m -> m
                        Nothing -> Tuple (Move 0 O) 0
  
comp :: (Tuple Move Int) -> (Tuple Move Int) -> Ordering 
comp (Tuple m x) (Tuple m' y) = compare x y
