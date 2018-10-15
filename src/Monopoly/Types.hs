module Monopoly.Types where

import qualified Data.Map as Map
import qualified Data.Array as Array

type Money = Float
type PlayerID = Int
type BoardIndex = Integer
type PlotName = String

-- Each plot is grouped by a color. Groups can be sorted from cheapest to most expensive.
data PlotGroup = Brown
               | LightBlue
               | Purple
               | Orange
               | Red
               | Yellow
               | Green
               | Blue
               deriving (Eq, Ord)

-- Plot owner can build houses and hotels to plot. Plot items can be sorted: a hotel is more valuable than a house.
data PlotItem = House
              | Hotel
              deriving (Eq, Ord)

-- The game board consists cells where players visit. Each cell have different action in a game.
data Cell = Go Money
          | Plot PlotOwnership PlotName PlotGroup
          | ElectricCompany PlotOwnership
          | WaterWorks PlotOwnership
          | Railroad PlotOwnership PlotName
          | CommunityChest
          | Change
          | IncomeTax Money
          | LuxuryTax Money
          | VisitJail
          | FreeParking
          | GoToJail
          deriving Eq

-- Plots can be bought. If a plot is owned by a player, all other players have to pay rent if they visit the plot.
-- TODO: add rent information.
data PlotOwnership = FreePlot Money
                   | OwnedPlot Money PlayerID [PlotItem]
                   | ReclaimedPlot Money PlayerID [PlotItem]
                   deriving (Eq, Ord)

-- Community chest cards are most likely to give you money.
data CommunityChestCard =
                          -- Get out of Jail Free. This card may be kept until needed, or traded/sold.
                          GetOutOfJailFreeCC

                          -- Go to Jaill. Go directly to Jail. Do not pass GO, do not collect $200.
                        | GoToJailCC
                        deriving (Eq, Enum, Bounded)

-- Change cards are more likely to move players, often with lethal consequences.
data ChangeCard =
                  -- Get out of Jail Free. This card may be kept until needed, or traded/sold.
                  GetOutOfJailFreeC

                  -- Go to Jaill. Go directly to Jail. Do not pass GO, do not collect $200.
                | GoToJailC
                deriving (Eq, Enum, Bounded)

-- Player data contains where the player is located at the board,
-- how much money he/she has and what cards (community chest or change)
-- the player is currently holding.
data Player = Player
   { location :: BoardIndex
   , bankBalance :: Money
   , heldCommunityChestCards :: [CommunityChestCard]
   , heldChangeCards :: [ChangeCard] }

-- Game board contains the game state. It will change during the game to reflect game actions.
data Board = Board
   -- Board structure.
   { cells :: Array.Array BoardIndex Cell

   -- Deck for community chest cards.
   , communityChestCards :: [CommunityChestCard]

   -- Deck for change cards.
   , changeCards :: [ChangeCard]

   -- Maps player IDs to player data.
   , players :: Map.Map PlayerID Player }
