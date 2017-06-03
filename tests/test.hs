import Test.Hspec

main :: IO ()
main = hspec . parallel $ it "is" pending
