
import TimeSpec(testTime)
import ParserSpec(testParser)
import ReadMetricsSpec(testReadMetrics)

main :: IO () 
main = do
  testTime
  testParser
  testReadMetrics