import Action(finish)
import DealerProg(toProgram)
import qualified Terminology as T
import Bids.StandardOpenings(b1C)
--import Bids.StandardModernPrecision.BasicBids(b1D)


main :: IO ()
main = putStrLn . toProgram . snd . finish T.South $ b1C
