import MondoTest
import Test.Hspec

main :: IO ()
main = hspec $ do
    MondoTest.run
