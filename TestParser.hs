{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module TestParser where

import U.SrcLoc
import U.FastString
import U.StringBuffer
import U.Outputable

import AST
import Parser
import Lexer
import Printer ()

-- showed parsed expression
psExp :: String -> String
psExp = show . runParser 100

-- pretty printed parsed expression
ppExp :: String -> String
ppExp = showSDoc 100 . ppr . runParser 100

runParser :: Int -> String -> ExpPS
runParser f str = case unP parseExpression parseState of
                    POk     _ r -> r
                    PFailed s e -> error ("Error at " ++ show s ++ ":\n" ++
                                          showSDoc f e)
  where
    filename   = "<interactive>"
    location   = mkRealSrcLoc (mkFastString filename) 1 1
    buff       = stringToStringBuffer str
    parseState = mkPState buff location
