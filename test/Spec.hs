
import TimeSpec(testTime)
import ParserSpec(testParser)
import AppSpec(testApp)

main :: IO () 
main = do
  testTime
  testParser
  testApp
  -- TODO Add config tests