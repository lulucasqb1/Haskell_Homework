{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Distribution.Compat.Lens (_1)

charToMT :: Char -> MessageType
charToMT 'I' = MessageType Info
charToMT 'W' = MessageType Warning
charToMT 'E' = MessageType Error
charToMT _ = Nothing

parseMessage :: String -> LogMessage
parseMessage x = case words x of
        (first : second : rest) -> (first == 'I') || (first == 'W') || (first == 'E') && (isDigit second) -> LogMessage (charToMT m) charToIn ts s
        _ -> LogMessage Unknown _







