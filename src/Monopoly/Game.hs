module Monopoly.Game (evalMonopoly, runMonopoly, mkBoard) where

import Monopoly.Types
import Control.Monad.Trans.State
import Data.Array
import qualified Data.Map as Map

type Monopoly = State Board

-- Action planner that generates possible actions in current situation.
evalMonopoly :: Enum a => a -> Monopoly [Action]
evalMonopoly player = do
   board <- get

   let playerId = fromEnum player

   return $ case Map.lookup playerId $ players board of

      -- Evaluate cell actions for player.
      Just x -> let loc = location x
                in cellActions playerId loc $ cells board ! loc

      -- Unknown player, no actions.
      _ -> []

cellActions :: PlayerID -> BoardUnit -> Cell -> [Action]
cellActions player loc (Go x) =
   [ ReceiveMoney player x ]

cellActions player loc (Plot (FreePlot x) _ group) =
   [ BuyPlot player loc ]

cellActions player loc (ElectricCompany (FreePlot x)) =
   [ BuyPlot player loc ]

cellActions player loc (WaterWorks (FreePlot x)) =
   [ BuyPlot player loc ]

cellActions player loc (Railroad (FreePlot x) _) =
   [ BuyPlot player loc ]

cellActions player _ CommunityChest =
   [ DrawCommunityChestCard player ]

cellActions player _ Change =
   [ DrawChangeCard player ]

cellActions player _ (IncomeTax x) =
   [ GiveMoney player x ]

cellActions player _ (LuxuryTax x) =
   [ GiveMoney player x ]

cellActions player _ GoToJail =
   [ StayInJail player ]

cellActions _ _ _ =
   []

-- Runs an action.
runMonopoly :: Action -> Monopoly ()
runMonopoly _ =
   error "not implemented"

mkBoard :: Enum a => a -> Board
mkBoard firstPlayer =
   Board
      { cells = listArray (1, 40)
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

      -- TODO: shuffle community chest cards.
      , communityChestCards = [ (minBound :: CommunityChestCard).. ]

      -- TODO: shuffle change cards.
      , changeCards = [ (minBound :: ChangeCard).. ]

      , players = Map.fromList $ map (mkPlayer . fromEnum) [ firstPlayer.. ] }

mkPlayer :: PlayerID -> (PlayerID, Player)
mkPlayer =
   flip (,) $ Player
      { location = 1
      , bankBalance = 15000
      , heldCommunityChestCards = []
      , heldChangeCards = [] }
