import Test.Tasty
import Test.Tasty.QuickCheck

import Instances(Namelist(..))

import Text.Namelist

main :: IO ()
main = defaultMain $ testGroup ""
    [ testProperty "id = parse . pretty" $ \(Namelist gs) ->
        (parse . pretty) gs === Right gs
    , testProperty "id = parse . prettyCompact" $ \(Namelist gs) ->
        (parse . prettyCompact) gs === Right gs
    ]
