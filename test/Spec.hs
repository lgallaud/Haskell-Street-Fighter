import Test.Hspec
import CoordSpec as CS
import HitboxSpec as HS
import CombatSpec as CBS


main :: IO ()
main = hspec $ do
    CS.coordSpecGD
    HS.makeHitboxInv
    CBS.setTest
