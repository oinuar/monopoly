module Monopoly.Types where

import qualified Data.Map as Map
import qualified Data.Array as Array

type Money = Float
type PlayerID = Int
type BoardUnit = Integer
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
               deriving (Eq, Ord, Show)

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
          | GoToJail
          | VisitJail
          | FreeParking
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

-- Actions that can be taken in a game.
data Action =

            -- Throws a dice.
              ThrowDice PlayerID [ Integer ]

            -- Receives or gives out money.
            | BankTransaction PlayerID Money

            -- Buys a plot at given board index.
            | BuyPlot PlayerID BoardUnit

            -- Buys a plot item for plot at given board index.
            | BuyPlotItem PlayerID BoardUnit PlotItem

            -- Draws community chest card from deck.
            | DrawCommunityChestCard PlayerID

            -- Draws change card from deck.
            | DrawChangeCard PlayerID

            -- Keeps community chest card that was drawn from the deck.
            | KeepCommunityChestCard PlayerID CommunityChestCard

            -- Keeps change card that was drawn from the deck.
            | KeepChangeCard PlayerID ChangeCard

            -- Uses previously kept community chest card and puts it back to deck.
            | UseCommunityChestCard PlayerID CommunityChestCard

            -- Uses previously kept change card and puts it back to deck.
            | UseChangeCard PlayerID ChangeCard

            -- Stays in jail.
            | StayInJail PlayerID

-- When player draws a card from deck, he/she first holds the card and
-- then can decide to keep it for later use.
data PlayerCard a = HoldCard a
                  | KeepCard a

-- Player data contains where the player is located at the board,
-- how much money he/she has and what cards (community chest or change)
-- the player is currently holding.
data Player = Player
   { locationOf :: BoardUnit
   , bankBalanceOf :: Money
   , communityChestCardsHeldBy :: [ PlayerCard CommunityChestCard ]
   , changeCardsHeldBy :: [ PlayerCard ChangeCard ] }

-- Game board contains the game state. It will change during the game to reflect game actions.
data Board = Board

   -- Board structure.
   { cellsOf :: Array.Array BoardUnit Cell

   -- Deck for community chest cards.
   , communityChestCardsOf :: [ CommunityChestCard ]

   -- Deck for change cards.
   , changeCardsOf :: [ ChangeCard ]

   -- Maps player IDs to player data.
   , playersOf :: Map.Map PlayerID Player }

isHoldingCard :: PlayerCard a -> Bool
isHoldingCard HoldCard{} =
   True

isHoldingCard _ =
   False

card :: PlayerCard a -> a
card (HoldCard x) =
   x

card (KeepCard x) =
   x

holdCard :: Eq a => a -> PlayerCard a -> PlayerCard a
holdCard x0 (KeepCard x1)
   | x0 == x1 = HoldCard x1
   | otherwise = KeepCard x1

holdCard _ x =
   x

keepCard :: Eq a => a -> PlayerCard a -> PlayerCard a
keepCard x0 (HoldCard x1)
   | x0 == x1 = KeepCard x1
   | otherwise = HoldCard x1

keepCard _ x =
   x
