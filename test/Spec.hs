module Main
    ( main
    ) where

-- import qualified Spec.Model
import qualified Spec.Trace
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "trace testing"
    [ Spec.Trace.tests
    --, Spec.Model.tests
    ]