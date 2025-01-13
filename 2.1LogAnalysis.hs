{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
module LogAnalysis where
import Log
import Data.Char (isDigit)

charToMT :: String ->  MessageType
charToMT "I" = Info
charToMT "W" = Warning
charToMT "E" = Error 1
charToMT _ = Info

parseMessage :: String -> LogMessage
parseMessage x = case words x of
        (first : second : third :rest) 
                |(elem first ["I","W"]) && (all isDigit second) -> LogMessage (charToMT first) (read second) (unwords (third:rest))
                |(first == "E") && (all isDigit second) && (all isDigit third) -> LogMessage (Error (read second)) (read third) (unwords rest) 
        _ -> Unknown x 







