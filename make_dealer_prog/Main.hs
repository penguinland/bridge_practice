-- The intention with this executable is to print out the dealer program for a
-- certain auction. You can then modify the program to do various other one-off
-- things to investigate the situation. Example questions that this helps with:
--   - How often does a Precision 1D opener have natural diamonds?
--   - After a weak two opening, how often does the other partnership have
--     game-going strength? What about a weak two opening in second seat?
--   - Suppose we have the auction we had at the club last week. How often do we
--     have a spade stopper?

import Action(finish)
import DealerProg(toProgram)
import qualified Terminology as T

--import Bids.StandardOpenings(b1C)
import Bids.StandardModernPrecision.BasicBids(b1D)


main :: IO ()
main = let
    auction = b1D
  in
    putStrLn . toProgram . snd . finish T.South $ auction
