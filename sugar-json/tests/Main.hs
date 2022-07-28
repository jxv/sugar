import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

main :: IO ()
main = do
    test <- testSpec "sugar" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
