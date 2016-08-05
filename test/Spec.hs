import Control.Zipper.Simple
import Test.Hspec

import Data.Maybe (fromJust)
import Control.Monad ((=<<))

import Control.Lens

fromRight :: Either e a -> a
fromRight (Right x) = x
fromRight _ = error "fromRight (Left ...)"

fromLeft :: Either e a -> e
fromLeft (Left x) = x
fromLeft _ = error "fromLeft (Right ...)"

main :: IO ()
main = hspec $ do
  describe "Root" $ do
    it "is able to adjust the value at its focus and rezip" $
      False == (rezip $ over focus not $ root True)

  describe "==>" $ do
    it "is able to adjust itself and be rezipped" $
      (True, "Hello") == (rezip $ over focus not $ descendLens _1 $ root (False, "Hello"))

  describe "=*=>" $ do
    it "is able to adjust itself and be rezipped" $
      [True] == (rezip $ over focus not $ fromJust $ descendList $ root [False])

    describe "rightward" $
      it "is able to go rightward" $
        "H1llo" == (rezip $ set focus '1' $ fromJust $ rightward =<< (descendList $ root "Hello"))

    describe "leftward" $
      it "is able to go rightward and then leftward" $
        "1ello" == (rezip $ set focus '1' $ fromJust $ leftward =<< rightward =<< (descendList $ root "Hello"))

    describe "delete" $ do
      it "is able to delete its current focus in the middle" $
        "Hllo" == (rezip $ fromRight $ deleteFocus $ fromJust $ rightward =<< (descendList $ root "Hello"))

      it "is able to delete its current focus when nothing is leftward" $
        "" == (rezip $ fromLeft $ deleteFocus $ fromJust $ descendList $ root "a")

      it "is able to delete its current focus when it's pointing at the rightward-most element" $
        "He" == (rezip $ fromRight $ deleteFocus $ fromJust $ rightward =<< rightward =<< (descendList $ root "Hel"))
