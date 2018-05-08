import Data.List.Utils
import Structures(Hand(..))
import Output(toLatex)
import qualified Terminology

main :: IO ()
main = putStrLn . toLatex $ Hand "A K Q" "J 10 9" "8 7 6" "5 4 3 2"
--main = putStrLn . toLatex $ Terminology.Spades
