import System.Random (randomRIO)  -- For random number generation
import Data.List (delete, sort, groupBy, maximumBy)  -- Utilities for list manipulation
import Data.Ord (comparing)  -- For ordering comparisons
import Data.List.Split (chunksOf) -- To split lists into smaller chunks

-- Define card suits
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum, Bounded)  -- Automatically derive useful instances

-- Define card ranks
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded, Ord)  -- Automatically derive useful instances

-- Define a playing card as a combination of rank and suit
data Card = Card Rank Suit
  deriving (Show, Eq)

-- Define a deck as a list of cards
type Deck = [Card]

-- Define a player in the game
data Player = Player
  { playerName      :: String   -- Name of the player
  , playerHand      :: [Card]   -- The player's hand (cards)
  , playerChips     :: Int      -- The number of chips the player has
  , isDealer        :: Bool     -- Whether the player is the dealer
  , playingStrategy :: String   -- Strategy: Passive, Aggressive, Smart, Random
  } deriving (Show)

-- Define the game's state
data GameState = GameState
  { activePlayers  :: [Player]  -- Players currently in the game
  , deck           :: Deck      -- Remaining cards in the deck
  , communityCards :: [Card]    -- Community cards on the table
  , pot            :: Int       -- Total chips in the pot
  , bets           :: [(String, Int)]  -- (Player Name, Bet Amount)
  , dealerPos      :: Int       -- Index of the dealer
  , smallBlindPos  :: Int       -- Index of the small blind
  , bigBlindPos    :: Int       -- Index of the big blind
  } deriving (Show)

-- Generate a full deck of cards (all ranks and suits)
fullDeck :: Deck
fullDeck = [Card rank suit | rank <- [Two .. Ace], suit <- [Hearts .. Spades]]

-- Shuffle the deck using random index selection
shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []  -- Base case: empty deck remains empty
shuffleDeck d = do
  randIndex <- randomRIO (0, length d - 1)  -- Generate a random index
  let card = d !! randIndex  -- Select the card at the random index
  rest <- shuffleDeck (delete card d)  -- Recurse on the remaining cards
  return (card : rest)  -- Prepend the selected card to the shuffled deck

-- Deal hole cards (2 cards per player)
dealHoleCards :: Int -> Deck -> ([Card], Deck)
dealHoleCards numPlayers d =
  -- Split the deck into the cards to deal and the remaining deck
  let (dealt, remaining) = splitAt (2 * numPlayers) d
  in (dealt, remaining)  -- Return the dealt cards and the updated deck


-- Deal community cards
dealCommunityCards :: Int -> Deck -> ([Card], Deck)
dealCommunityCards numCards d =
  -- Split the deck into dealt cards and the remaining deck
  let (dealt, remaining) = splitAt numCards d
  in (dealt, remaining)  -- Return the dealt cards and the updated deck

-- Evaluate the strength of a hand
data HandRank = HighCard [Rank]
              | OnePair Rank
              | TwoPair Rank Rank
              | ThreeOfAKind Rank
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank
              | StraightFlush Rank
  deriving (Show, Eq, Ord)  -- Allow ranking of hands for comparison

-- Determine the rank of a given hand
rankHand :: [Card] -> HandRank
rankHand cards = 
  -- Extract ranks and group identical ones
  let ranks = sort [rank | Card rank _ <- cards]  -- Sort the ranks
      groupedRanks = groupBy (==) ranks  -- Group consecutive identical ranks
      rankCounts = map length groupedRanks  -- Count occurrences of each rank
  in if length groupedRanks == 2 then
       -- Two groups indicate either Four of a Kind or Full House
       if rankCounts == [4, 1] then
         FourOfAKind (head (groupedRanks !! 0))  -- Four of a Kind
       else if rankCounts == [3, 2] then
         FullHouse (head (groupedRanks !! 0)) (head (groupedRanks !! 1))  -- Full House
       else
         HighCard ranks  -- Fallback for unhandled cases
     else if length groupedRanks == 3 then
       -- Three groups indicate either Three of a Kind or Two Pair
       if rankCounts == [3, 1, 1] then
         ThreeOfAKind (head (groupedRanks !! 0))  -- Three of a Kind
       else if rankCounts == [2, 2, 1] then
         TwoPair (head (groupedRanks !! 0)) (head (groupedRanks !! 1))  -- Two Pair
       else
         HighCard ranks  -- Fallback for unhandled cases
     else if length groupedRanks == 4 then
       -- Four groups indicate a single pair
       OnePair (head (groupedRanks !! 0))  -- One Pair
     else
       -- Default case for unranked hands
       HighCard ranks  -- High Card (lowest rank)

-- Determine the winner among the active players
determineWinner :: [Player] -> [Card] -> Player
determineWinner [] _ = error "No players left to determine a winner!"  -- Error if no players
determineWinner players communityCards =
  -- Combine each player's hand with the community cards and rank their hands
  let playerHands = [(p, rankHand (playerHand p ++ communityCards)) | p <- players]
  in if null players then
       -- Handle edge case where the players list is empty
       error "No players left to determine a winner!" 
     else
       -- Find the player with the highest ranked hand
       let bestPlayer = maximumBy (comparing snd) playerHands
       in fst bestPlayer  -- Return the player with the best hand




-- Process the action of a player during a betting round
processPlayer :: Player -> Int -> IO (String, Int)
processPlayer p lastBet
  | playingStrategy p == "Passive"   = processPassive p lastBet  -- Passive strategy
  | playingStrategy p == "Aggressive" = processAggressive p      -- Aggressive strategy
  | playingStrategy p == "Smart"      = processSmart p lastBet   -- Smart strategy
  | playingStrategy p == "Random"     = processRandom p          -- Random strategy
  | otherwise                         = return (playerName p, 0) -- Default: fold with no bet

-- Handle a passive player's actions
processPassive :: Player -> Int -> IO (String, Int)
processPassive p lastBet
  | lastBet <= playerChips p = do
      -- The player can afford to call the last bet
      if playerChips p == 0 then do 
        putStrLn $ playerName p ++ " folds due to insufficient chips."
        return (playerName p, 0)  -- Player folds
      else do
        let betAmount = lastBet  -- Match the last bet amount
        putStrLn $ playerName p ++ " calls and matches the last bet of " ++ show betAmount ++ " chips."
        return (playerName p, betAmount)  -- Player calls
  | otherwise = do
      -- The player can't afford the last bet
      putStrLn $ playerName p ++ " folds due to insufficient chips."
      return (playerName p, 0)  -- Player folds

-- Handle an aggressive player's actions
processAggressive :: Player -> IO (String, Int)
processAggressive p =
  if playerChips p > 0 then do
    -- Aggressive player bets a random amount between 5 and 40 chips
    let maxBet = playerChips p `div` 2
    randomBet <- randomRIO (5, max 5 (min 40 maxBet))
    let betAmount = min randomBet (playerChips p) -- Ensure bet does not exceed available chips
    putStrLn $ playerName p ++ " aggressively bets " ++ show betAmount ++ " chips."
    return (playerName p, betAmount)  -- Player bets
  else do
    -- Aggressive player folds if no chips are left
    putStrLn $ playerName p ++ " folds."
    return (playerName p, 0)

-- Handle a smart player's actions based on the game situation
processSmart :: Player -> Int -> IO (String, Int)
processSmart p lastBet = do
  let availableChips = playerChips p  -- Get the player's available chips

  if availableChips <= lastBet then do
    -- Player can't afford to match the last bet
    putStrLn $ playerName p ++ " folds due to insufficient chips to match the last bet of " ++ show lastBet ++ " chips."
    return (playerName p, 0)  -- Player folds
  else if availableChips > 50 && lastBet < 15 then do
    -- Favorable situation, therefore Player bets a small amount
    let betAmount = min availableChips (lastBet + 20)
    putStrLn $ playerName p ++ " has a favorable situation and bets " ++ show betAmount ++ " chips."
    return (playerName p, betAmount)
  else if availableChips > 20 then do
    -- Player has moderate chips, matches the last bet
    let betAmount = lastBet
    putStrLn $ playerName p ++ " calls with " ++ show betAmount ++ " chips."
    return (playerName p, betAmount)
  else do
    -- Player has low chips, chooses to fold
    putStrLn $ playerName p ++ " folds to conserve chips."
    return (playerName p, 0)

-- Handle a random player's actions
processRandom :: Player -> IO (String, Int)
processRandom p = do
  randomAction <- randomRIO (0, 1) :: IO Int  -- Generate a random decision (0 or 1)
  if randomAction == 1 then do
    -- Random player decides to bet
    randomBet <- randomRIO (1, playerChips p)  -- Bet a random amount
    let betAmount = min (playerChips p) randomBet
    putStrLn $ playerName p ++ " randomly decides to bet " ++ show betAmount ++ " chips."
    return (playerName p, betAmount)  -- Player bets
  else do
    -- Random player decides to fold
    putStrLn $ playerName p ++ " folds."
    return (playerName p, 0)



-- Conduct a betting round
bettingRound :: GameState -> IO GameState
bettingRound gameState = do
  -- Get the list of active players
  let players = activePlayers gameState
  putStrLn "Betting round starts!"  -- Announce the start of the betting round

  let initialBet = 0  -- Start with no bets placed
  
  -- Process each player's turn and collect their bets
  updatedBets <- bettingRoundHelper players [] initialBet

  -- Calculate the total pot after all bets are placed
  let updatedPot = pot gameState + sum (map snd updatedBets)

  -- Update each player's chips based on their bet
  let updatedPlayers = zipWith
        (\player bet -> if snd bet == 0
                         then player  -- Player folds, no change to chips
                         else player {playerChips = max 0 (playerChips player - snd bet)})  -- Deduct bet amount
        players updatedBets

  -- Remove players who folded (bet 0) from the active players list
  let remainingPlayers = [p | (p, (_, bet)) <- zip updatedPlayers updatedBets, bet > 0]

  putStrLn "Betting round ends!"  -- Announce the end of the betting round

  -- Display the updated status of each player
  mapM_ printPlayerStatus remainingPlayers

  -- Return the updated game state with new pot and active players
  return gameState {activePlayers = remainingPlayers, pot = updatedPot}

-- Helper function to process each player's turn during a betting round
bettingRoundHelper :: [Player] -> [(String, Int)] -> Int -> IO [(String, Int)]
bettingRoundHelper [] betsSoFar _ = return betsSoFar  -- Base case: no more players to process
bettingRoundHelper (player:rest) betsSoFar lastBet = do
  -- Process the current player's action
  (name, bet) <- processPlayer player lastBet
  
  -- Update the list of bets with the current player's bet
  let updatedBets = betsSoFar ++ [(name, bet)]

  -- Update the last bet amount if the current player raised
  let newLastBet = if bet > lastBet then bet else lastBet

  -- Recursively process the remaining players
  bettingRoundHelper rest updatedBets newLastBet

-- Print the status of a player (name and remaining chips)
printPlayerStatus :: Player -> IO ()
printPlayerStatus p =
  putStrLn $ playerName p ++ " has " ++ show (playerChips p) ++ " chips remaining."

-- Reshuffle the deck if there are not enough cards for dealing
reshuffleDeck :: GameState -> IO Deck
reshuffleDeck gameState = do
  -- Combine used cards from players' hands and community cards with the remaining deck
  let usedCards = concatMap playerHand (activePlayers gameState) ++ communityCards gameState
  
  -- Shuffle the combined deck and return it
  shuffleDeck (usedCards ++ deck gameState)



playRound :: GameState -> IO GameState
playRound gameState
  -- If no players are active, end the game
  | null (activePlayers gameState) = do
      putStrLn "No players left! The game ends."
      return gameState
  -- Otherwise, continue with the round
  | otherwise = do
      -- Deal hole cards to active players
      let (holeCards, deck1) = dealHoleCards (length (activePlayers gameState)) (deck gameState)
      let updatedPlayers = zipWith (\p hand -> p {playerHand = hand}) (activePlayers gameState) (chunksOf 2 holeCards)
      let gameState1 = gameState {activePlayers = updatedPlayers, deck = deck1}
      putStrLn "Hole cards dealt!"
      mapM_ printPlayerHands updatedPlayers

      -- Conduct the first betting round
      gameState2 <- bettingRound gameState1

      -- Deal the Flop
      putStrLn $ "Deck size before dealing the Flop: " ++ show (length (deck gameState2))
      deckAfterFlop <- if length (deck gameState2) < 3
                          then do
                            putStrLn "Not enough cards for the Flop, reshuffling..."
                            reshuffledDeck <- reshuffleDeck gameState2
                            let (flop, deckAfter) = dealCommunityCards 3 reshuffledDeck
                            return (flop, deckAfter)
                          else do
                            let (flop, deckAfter) = dealCommunityCards 3 (deck gameState2)
                            return (flop, deckAfter)
      let gameState3 = gameState2 {communityCards = fst deckAfterFlop, deck = snd deckAfterFlop}
      putStrLn $ "Flop: " ++ show (fst deckAfterFlop)
      gameState4 <- bettingRound gameState3

      -- Deal the Turn
      newDeck1 <- if length (deck gameState4) < 1 then reshuffleDeck gameState4 else return (deck gameState4)
      let (turn, deck3) = dealCommunityCards 1 newDeck1
      let gameState5 = gameState4 {communityCards = communityCards gameState4 ++ turn, deck = deck3}
      putStrLn $ "Turn: " ++ show turn
      gameState6 <- bettingRound gameState5

      -- Deal the River
      newDeck2 <- if length (deck gameState6) < 1 then reshuffleDeck gameState6 else return (deck gameState6)
      let (river, deck4) = dealCommunityCards 1 newDeck2
      let gameState7 = gameState6 {communityCards = communityCards gameState6 ++ river, deck = deck4}
      putStrLn $ "River: " ++ show river
      gameState8 <- bettingRound gameState7

      -- Determine the winner or end the round
      if null (activePlayers gameState8)
        then do
          putStrLn "No players left after this round! The game ends."
          return gameState8
        else do
          let winner = determineWinner (activePlayers gameState8) (communityCards gameState8)
          let winnerRank = rankHand (playerHand winner ++ communityCards gameState8)
          mapM_ (printFinalHand (communityCards gameState8)) (activePlayers gameState8)
          putStrLn $ "Winner of the round: " ++ playerName winner ++ " with hand rank: " ++ show winnerRank
          return gameState8
  where
    -- Print the hole cards for each player
    printPlayerHands :: Player -> IO ()
    printPlayerHands p =
      putStrLn $ playerName p ++ " has " ++ show (playerHand p) ++ " as their hand."

    -- Print the final hand (including community cards) for each player
    printFinalHand :: [Card] -> Player -> IO ()
    printFinalHand communityCards p = do
      let fullHand = playerHand p ++ communityCards
      putStrLn $ playerName p ++ "'s final hand (including community cards): " ++ show fullHand

-- Main game loop
gameLoop :: GameState -> Int -> IO ()
gameLoop gameState roundNum
  -- Check if there are no players left with chips
  | null (activePlayers gameState) = putStrLn "No players left with chips! The game ends."
  -- If only one player remains, declare them the winner
  | length (activePlayers gameState) == 1 = putStrLn $ playerName (head (activePlayers gameState)) ++ " wins the game!"
  -- If the round limit is reached, determine the winner by chip count
  | roundNum > 100 = do
      let winner = maximumBy (comparing playerChips) (activePlayers gameState)
      putStrLn $ playerName winner ++ " has the most chips and wins after 100 rounds!"
  -- Otherwise, play a new round and continue the game loop
  | otherwise = do
      putStrLn $ "ROUND " ++ show roundNum
      newGameState <- playRound gameState
      gameLoop newGameState (roundNum + 1) -- Recursively call gameLoop for the next round

-- Simulate a large number of games
simulateGames :: Int -> IO ()
simulateGames numGames = simulateGamesHelper numGames [] 1

-- Helper function for simulating games one by one
simulateGamesHelper :: Int -> [(String, Int)] -> Int -> IO ()
simulateGamesHelper 0 results _ = do
  -- After all games are simulated, summarize the results
  putStrLn "\nSimulation Results:"
  summarizeResults results
simulateGamesHelper remainingGames results gameNumber = do
  -- Simulate a single game
  putStrLn $ "\nStarting game #" ++ show gameNumber
  winner <- simulateSingleGame
  let updatedResults = updateResults winner results
  simulateGamesHelper (remainingGames - 1) updatedResults (gameNumber + 1)

-- Update the results list with the winner of the current game
updateResults :: String -> [(String, Int)] -> [(String, Int)]
updateResults winner [] = [(winner, 1)]
updateResults winner ((player, wins):rest)
  | player == winner = (player, wins + 1) : rest
  | otherwise = (player, wins) : updateResults winner rest

-- Summarize the results of all simulated games
summarizeResults :: [(String, Int)] -> IO ()
summarizeResults results =
  mapM_ (\(player, wins) -> putStrLn $ player ++ " won " ++ show wins ++ " games.") results

-- Simulate a single game
simulateSingleGame :: IO String
simulateSingleGame = do
  shuffledDeck <- shuffleDeck fullDeck
  let players = [Player {playerName = "Udo (Random)", playerHand = [], playerChips = 200, isDealer = False, playingStrategy = "Random"},
                 Player {playerName = "Michael (Aggressive)", playerHand = [], playerChips = 200, isDealer = True, playingStrategy = "Aggressive"},
                 Player {playerName = "Chiedozie (Passive)", playerHand = [], playerChips = 200, isDealer = False, playingStrategy = "Passive"},
                 Player {playerName = "Lionel (Smart)", playerHand = [], playerChips = 200, isDealer = False, playingStrategy = "Smart"}]
  let initialGameState = GameState {activePlayers = players, deck = shuffledDeck, communityCards = [], pot = 0, bets = [], dealerPos = 0, smallBlindPos = 1, bigBlindPos = 2}
  finalGameState <- gameLoopSimulation initialGameState 1
  let winner = if null (activePlayers finalGameState)
               then "No one"
               else playerName (head (activePlayers finalGameState))
  putStrLn $ "Winner of this game: " ++ winner
  return winner

-- Game loop for simulation
gameLoopSimulation :: GameState -> Int -> IO GameState
gameLoopSimulation gameState roundNum
  | null (activePlayers gameState) = return gameState
  | length (activePlayers gameState) == 1 = return gameState
  | roundNum > 100 = return gameState
  | otherwise = do
      newGameState <- playRound gameState
      gameLoopSimulation newGameState (roundNum + 1)

-- Main function to start the simulation
main :: IO ()
main = do
  putStrLn "Starting a simulation of Texas Hold'em games with 4 players."
  simulateGames 10  -- Simulate 10 games



