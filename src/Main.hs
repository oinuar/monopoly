{-# LANGUAGE JavaScriptFFI #-}

import Monopoly.Types
import Monopoly.Game
import Data.Array (elems)
import Control.Monad (mapM_)
import Data.JSString (JSString, pack)

data GamePlayer = Shoe deriving Enum

main :: IO ()
main = do
   let board = mkBoard Shoe

   mapM_ addCell $ elems (cellsOf board)

addCell :: Cell -> IO ()
addCell Go{} =
   addGoCell

addCell (Plot (FreePlot value) name group) =
   addPlotCell (pack $ show group) (pack name) (pack $ show value)

addCell (ElectricCompany (FreePlot value)) =
   addElectricCompanyCell (pack $ show value)

addCell (WaterWorks (FreePlot value)) =
   addWaterWorksCell (pack $ show value)

addCell (Railroad (FreePlot value) name) =
   addRailroadCell (pack name) (pack $ show value)

addCell CommunityChest =
   addCommunityChestCell

addCell Change =
   addChangeCell

addCell (IncomeTax x) =
   addIncomeTaxCell (pack $ show x)

addCell (LuxuryTax x) =
   addLuxuryTaxCell (pack $ show x)

addCell GoToJail =
   addGoToJailCell

addCell VisitJail =
   addVisitJailCell

addCell FreeParking =
   addFreeParkingCell


foreign import javascript unsafe "addGoCell();"
   addGoCell :: IO ()

foreign import javascript unsafe "addPlotCell($1, $2, $3);"
   addPlotCell :: JSString -> JSString -> JSString -> IO ()

foreign import javascript unsafe "addCommunityChestCell();"
   addCommunityChestCell :: IO ()

foreign import javascript unsafe "addChangeCell();"
   addChangeCell :: IO ()

foreign import javascript unsafe "addIncomeTaxCell($1);"
   addIncomeTaxCell :: JSString -> IO ()

foreign import javascript unsafe "addLuxuryTaxCell($1);"
   addLuxuryTaxCell :: JSString -> IO ()

foreign import javascript unsafe "addRailroadCell($1, $2);"
   addRailroadCell :: JSString -> JSString -> IO ()

foreign import javascript unsafe "addElectricCompanyCell($1);"
   addElectricCompanyCell :: JSString -> IO ()

foreign import javascript unsafe "addWaterWorksCell($1);"
   addWaterWorksCell :: JSString -> IO ()

foreign import javascript unsafe "addVisitJailCell();"
   addVisitJailCell :: IO ()

foreign import javascript unsafe "addFreeParkingCell();"
   addFreeParkingCell :: IO ()

foreign import javascript unsafe "addGoToJailCell();"
   addGoToJailCell :: IO ()
