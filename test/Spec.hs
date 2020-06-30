
import ParserSpec
import ReadMetricsSpec

--import Test.QuickCheck (property) -- TODO: check out quickcheck


main :: IO () 
main = do
  testParser
  testReadMetrics