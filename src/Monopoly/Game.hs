module Monopoly.Game (evalMonopoly, runMonopoly, mkBoard) where

import Monopoly.Types
import Control.Monad.Trans.State
import Data.Array
import System.Random
import Control.Monad (mapM, foldM)
import Data.List (splitAt, partition)
import qualified Data.Map as Map

type Monopoly = State Board

evalMonopoly :: Enum a => a -> Monopoly [Action]

-- Action planner that generates possible actions in current situation.
evalMonopoly player = do
   let playerId = fromEnum player
   player <- getPlayer playerId

   let loc = locationOf player
   cell <- getCell loc

   return $ mapExplicitCellActions playerId loc cell ++ mapPlayerActions playerId player

mapExplicitCellActions :: PlayerID -> BoardUnit -> Cell -> [Action]
mapExplicitCellActions playerId loc (Plot (FreePlot x) _ group) =
   [ BuyPlot playerId loc ]

mapExplicitCellActions playerId loc (ElectricCompany (FreePlot x)) =
   [ BuyPlot playerId loc ]

mapExplicitCellActions playerId loc (WaterWorks (FreePlot x)) =
   [ BuyPlot playerId loc ]

mapExplicitCellActions playerId loc (Railroad (FreePlot x) _) =
   [ BuyPlot playerId loc ]

mapExplicitCellActions playerId _ CommunityChest =
   [ DrawCommunityChestCard playerId ]

mapExplicitCellActions playerId _ Change =
   [ DrawChangeCard playerId ]

mapExplicitCellActions playerId _ (IncomeTax x) =
   [ BankTransaction playerId (-x) ]

mapExplicitCellActions playerId _ (LuxuryTax x) =
   [ BankTransaction playerId (-x) ]

mapExplicitCellActions playerId _ GoToJail =
   [ StayInJail playerId ]

mapExplicitCellActions _ _ _ =
   []

mapImplicitCellActions :: PlayerID -> BoardUnit -> Cell -> [Action]
mapImplicitCellActions playerId _ (Go x) =
   [ BankTransaction playerId x ]

mapImplicitCellActions _ _ _ =
   []

mapPlayerActions :: PlayerID -> Player -> [Action]
mapPlayerActions playerId player =
   let communityChestCards = communityChestCardsHeldBy player
       changeCards = changeCardsHeldBy player

   -- TODO: determine which cards can be kept and which kept cards can
   -- be used.
   in []


runMonopoly :: RandomGen g => g -> Action-> Monopoly g

runMonopoly g (ThrowDice playerId xs)

   -- Throw dice the first time.
   | length xs `mod` 2 == 0 = do
      player <- getPlayer playerId

      let loc = locationOf player
      let (number, g') = randomR (1, 6) g

      runMonopoly g' $ ThrowDice playerId (number : xs)

   -- Throw dice the second time.
   | otherwise = do
      player <- getPlayer playerId

      let loc = locationOf player
      let (number, g') = randomR (1, 6) g

      -- Check if player gets same dice numbers in both throws.
      if length xs > 2 && all ((==) number) xs

         -- Go to jail if player gets same dice numbers.
         then runMonopoly g $ StayInJail playerId

         -- Otherwise, move to cell.
         else do

            -- Restore all player cards that are not kept back to deck.
            restoreCommunityChestCardsToDeck playerId
            restoreChangeCardsToDeck playerId

            -- Move player to correct cell.
            actions <- mapM (move playerId) $ tail [ loc .. (loc + sum (take 2 xs)) ]

            -- Execute implicit actions and allow player to throw again if he/she got
            -- same dice numbers.
            foldM runMonopoly g $ concat actions ++ if number == head xs
                                                      then [ ThrowDice playerId (number : xs) ]
                                                      else []

-- Executes bank transaction that changes player's bank balance.
runMonopoly g (BankTransaction playerId amount) = do
   modifyPlayer playerId $ \x -> x { bankBalanceOf = bankBalanceOf x + amount }
   return g

-- Buys a plot and does nothing if plot cannot be bought.
runMonopoly g (BuyPlot playerId i) = do
   modifyCell i buy
   return g
   where
      buy (Plot (FreePlot x) name group) =
         Plot (OwnedPlot x playerId []) name group

      buy (ElectricCompany (FreePlot x)) =
         ElectricCompany (OwnedPlot x playerId [])

      buy (WaterWorks (FreePlot x)) =
         WaterWorks (OwnedPlot x playerId [])

      buy (Railroad (FreePlot x) name) =
         Railroad (OwnedPlot x playerId []) name

      buy x =
         x

-- Draws community chest card from deck.
runMonopoly g (DrawCommunityChestCard playerId) = do
   deck <- gets communityChestCardsOf
   let (i, g') = randomR (0, length deck - 1) g

   -- Split deck to two at random location.
   let (lowDeck, highDeck) = splitAt i deck

   -- Take element from high deck and make a new deck.
   modify $ \x -> x { communityChestCardsOf = lowDeck ++ tail highDeck }

   -- Give card that is on top of high deck to player.
   modifyPlayer playerId $ \x -> x { communityChestCardsHeldBy = (HoldCard $ head highDeck) : (communityChestCardsHeldBy x) }
   return g'

-- Draws change card from deck.
runMonopoly g (DrawChangeCard playerId) = do
   deck <- gets changeCardsOf
   let (i, g') = randomR (0, length deck - 1) g

   -- Split deck to two at random location.
   let (lowDeck, highDeck) = splitAt i deck

   -- Take element from high deck and make a new deck.
   modify $ \x -> x { changeCardsOf = lowDeck ++ tail highDeck }

   -- Give card that is on top of high deck to player.
   modifyPlayer playerId $ \x -> x { changeCardsHeldBy = (HoldCard $ head highDeck) : (changeCardsHeldBy x) }
   return g'

-- Keeps community chest card for later use.
runMonopoly g (KeepCommunityChestCard playerId card) = do
   modifyPlayer playerId $ \x -> x { communityChestCardsHeldBy = map (keepCard card) (communityChestCardsHeldBy x) }
   return g

-- Keeps change card for later use.
runMonopoly g (KeepChangeCard playerId card) = do
   modifyPlayer playerId $ \x -> x { changeCardsHeldBy = map (keepCard card) (changeCardsHeldBy x) }
   return g

restoreCommunityChestCardsToDeck :: PlayerID -> Monopoly ()
restoreCommunityChestCardsToDeck playerId = do
   player <- getPlayer playerId
   let (heldCards, keptCards) = partition isHoldingCard $ communityChestCardsHeldBy player

   modifyPlayer playerId $ \x -> x { communityChestCardsHeldBy = keptCards }
   modify $ \x -> x { communityChestCardsOf = communityChestCardsOf x ++ map card heldCards }

restoreChangeCardsToDeck :: PlayerID -> Monopoly ()
restoreChangeCardsToDeck playerId = do
   player <- getPlayer playerId
   let (heldCards, keptCards) = partition isHoldingCard $ changeCardsHeldBy player

   modifyPlayer playerId $ \x -> x { changeCardsHeldBy = keptCards }
   modify $ \x -> x { changeCardsOf = changeCardsOf x ++ map card heldCards }

move :: PlayerID -> BoardUnit -> Monopoly [Action]
move playerId units = do
   i <- wrap units
   modifyPlayer playerId $ \x -> x { locationOf = i }

   cell <- getCell i
   return $ mapImplicitCellActions playerId i cell

getCell :: BoardUnit -> Monopoly Cell
getCell units = do
   cells <- gets cellsOf
   return $ cells ! units

getPlayer :: PlayerID -> Monopoly Player
getPlayer playerId = do
   availablePlayers <- gets playersOf

   case Map.lookup playerId availablePlayers of
      Just x -> return x
      _ -> fail "Unknown player"

modifyCell :: BoardUnit -> (Cell -> Cell) -> Monopoly ()
modifyCell i f =
   modify $ \x -> x { cellsOf = let cells = cellsOf x
                                in cells // [ (i, f $ cells ! i) ] }

modifyPlayer :: PlayerID -> (Player -> Player) -> Monopoly ()
modifyPlayer playerId f = do
   player <- getPlayer playerId
   modify $ \x -> x { playersOf = Map.update (Just . f) playerId $ playersOf x }

wrap :: BoardUnit -> Monopoly BoardUnit
wrap units = do
   cells <- gets cellsOf
   return $ units `mod` (snd $ bounds cells)

mkBoard :: Enum a => a -> Board
mkBoard firstPlayer =
   Board
      { cellsOf = listArray (1, 40)
         [ Go 2000
         , Plot (FreePlot 60) "Mediter-Ranean Avenue" Brown
         , CommunityChest
         , Plot (FreePlot 60) "Baltic Avenue" Brown
         , IncomeTax 200
         , Railroad (FreePlot 200) "Reading Railroad"
         , Plot (FreePlot 100) "Oriental Avenue" LightBlue
         , Change
         , Plot (FreePlot 100) "Vermont Avenue" LightBlue
         , Plot (FreePlot 100) "Connecticut Avenue" LightBlue
         , VisitJail

         , Plot (FreePlot 140) "St. Charles Place" Purple
         , ElectricCompany (FreePlot 150)
         , Plot (FreePlot 140) "States Avenue" Purple
         , Plot (FreePlot 160) "Virginian Avenue" Purple
         , Railroad (FreePlot 200) "Pennyslavian Railroad"
         , Plot (FreePlot 180) "St. James Place" Orange
         , CommunityChest
         , Plot (FreePlot 180) "Tennessee Avenue" Orange
         , Plot (FreePlot 200) "New York Avenue" Orange
         , FreeParking

         , Plot (FreePlot 220) "Kentucky Avenue" Red
         , Change
         , Plot (FreePlot 220) "Indiana Avenue" Red
         , Plot (FreePlot 240) "Illinois Avenue" Red
         , Railroad (FreePlot 200) "B. & O. Railroad"
         , Plot (FreePlot 260) "Atlantic Avenue" Yellow
         , Plot (FreePlot 260) "Ventnor Avenue" Yellow
         , WaterWorks (FreePlot 150)
         , Plot (FreePlot 280) "Marvin Gardens" Yellow
         , GoToJail

         , Plot (FreePlot 300) "Pacific Avenue" Green
         , Plot (FreePlot 300) "North Carolina Avenue" Green
         , CommunityChest
         , Plot (FreePlot 300) "Pennyslvain Avenue" Green
         , Railroad (FreePlot 200) "Short Line"
         , Change
         , Plot (FreePlot 350) "Park Place" Blue
         , LuxuryTax 100
         , Plot (FreePlot 400) "Boardwalk" Blue ]

      , communityChestCardsOf = [ (minBound :: CommunityChestCard).. ]

      , changeCardsOf = [ (minBound :: ChangeCard).. ]

      , playersOf = Map.fromList $ map (mkPlayer . fromEnum) [ firstPlayer.. ] }

mkPlayer :: PlayerID -> (PlayerID, Player)
mkPlayer =
   flip (,) $ Player
      { locationOf = 1
      , bankBalanceOf = 15000
      , communityChestCardsHeldBy = []
      , changeCardsHeldBy = [] }
