{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "Lexer.x" #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lexer
       ( mkPState
       , lexer
       , P(..)
       , Token(..)
       , getPState
       , ParseResult(..)
       , AddAnn
       , addAnnsAt
       , addAnnotation
       , getSrcLoc
       , srcParseFail
       , failSpanMsgP
       , AnnKeywordId(..)
       , AnnotationComment(..)
       , IsUnicodeSyntax(..)
       , ApiAnnKey
       , unicodeAnn) where

import Control.Monad
import Control.Monad.Fail
import Data.Char
import Data.Maybe
import Data.Word
import Data.Data

import U.SrcLoc
import U.FastString
import U.Outputable
import U.StringBuffer


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0,55) [1,16,-44,-179,-182,100,-38,228,356,484,0,597,0,710,-119,0,-51,0,775,-138,160,-49,1031,967,0,1223,-14,1159,0,11,0,0,1405,1651,0,0,0,0,0,0,0,0,0,0,0,0,0,1906,2028,2150,2272,2394,2516,2638,77,1368]

alex_table :: Array Int Int
alex_table = listArray (0,2893) [0,31,53,50,44,46,29,26,45,35,30,36,29,29,29,19,29,3,32,0,0,29,29,29,29,30,34,29,29,29,0,33,0,29,0,0,0,0,0,0,0,37,38,29,0,39,20,43,29,54,54,54,54,54,54,54,54,54,54,21,0,6,40,0,0,0,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,0,41,0,0,50,0,50,50,50,50,50,50,50,50,51,50,50,49,50,50,50,50,50,50,50,50,50,50,50,50,50,50,2,54,54,54,54,54,54,54,54,54,54,0,0,0,0,2,23,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,0,4,55,55,55,55,55,55,55,55,55,55,0,0,0,0,42,0,0,0,0,14,25,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,22,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,5,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,7,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,18,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,55,55,55,55,55,55,55,55,55,55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,25,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,27,5,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18,8,12,12,12,13,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,22,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,23,7,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,9,10,10,10,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,52,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,50,50,50,50,50,50,50,50,50,48,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,0,0,0,0,0,0,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,0,0,0,0,50,0,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,47,50,50,50,50,50,50,53,53,53,0,0,0,53,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,53,0,0,0,0,0,0,0,0,53,53,53,53,53,53,53,53,53,53,0,0,0,0,0,0,0,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,0,0,0,0,53,0,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,2893) [-1,45,1,2,183,187,5,45,146,58,9,10,11,12,13,134,5,136,32,-1,-1,5,11,12,13,9,10,11,12,13,-1,45,-1,32,-1,-1,-1,-1,-1,-1,-1,40,41,32,-1,44,45,46,32,48,49,50,51,52,53,54,55,56,57,58,-1,45,61,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,92,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,123,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,45,-1,206,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,62,-1,-1,-1,-1,226,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,1,2,3,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,55) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,15,17,17,-1,24,24,28,28,-1,-1,-1,33,33,33,32,-1,32,32,-1,-1,-1,32,33,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,55) [AlexAccNone,AlexAcc (alex_action_6),AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccNone,AlexAccSkip,AlexAcc (alex_action_1),AlexAcc (alex_action_2),AlexAcc (alex_action_3),AlexAcc (alex_action_4),AlexAccSkip,AlexAcc (alex_action_7),AlexAcc (alex_action_8),AlexAcc (alex_action_9),AlexAcc (alex_action_10),AlexAcc (alex_action_11),AlexAcc (alex_action_12),AlexAcc (alex_action_13),AlexAcc (alex_action_14),AlexAcc (alex_action_15),AlexAcc (alex_action_16),AlexAcc (alex_action_17),AlexAcc (alex_action_18),AlexAcc (alex_action_19),AlexAcc (alex_action_20),AlexAcc (alex_action_21),AlexAcc (alex_action_21),AlexAcc (alex_action_21),AlexAcc (alex_action_21),AlexAcc (alex_action_22),AlexAcc (alex_action_23),AlexAcc (alex_action_24)]
{-# LINE 103 "Lexer.x" #-}

data Token
  = ITlet
  | ITin
  | ITdcolon       IsUnicodeSyntax
  | ITequal
  | ITlam          IsUnicodeSyntax
  | ITrarrow       IsUnicodeSyntax
  | ITdarrow       IsUnicodeSyntax
  | ITdot
  | ITocurly
  | ITccurly
  | ITvocurly
  | ITvccurly
  | IToparen
  | ITcparen
  | ITsemi
  | ITcomma
  | ITvarid        FastString
  | ITconid        FastString
  | ITinteger      String Integer
  | ITeof
  | ITlineComment  String
  | ITblockComment String
  deriving Show

instance Outputable Token where
  ppr x = text (show x)


type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

lineCommentToken :: Action
lineCommentToken = strtoken ITlineComment

nested_comment :: P (RealLocated Token) -> Action
nested_comment cont span buf len = do
  input <- getInput
  go (reverse $ lexemeToString buf len) (1::Int) input
  where
    go commentAcc 0 input = do
      setInput input
      docCommentEnd input commentAcc ITblockComment buf span

    go commentAcc n input = case alexGetChar' input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go ('\125':'-':commentAcc) (n-1) input -- '}'
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar' input of  -- '{' char
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

errBrace :: AlexInput -> RealSrcSpan -> P a
errBrace (AI end _) span = failLocMsgP (realSrcSpanStart span) end "unterminated `{-'"

varid :: Action
varid span buf len = return (L span (ITvarid fs))
  where
    !fs = lexemeToFastString buf len

conid :: StringBuffer -> Int -> Token
conid buf len = ITconid $! lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (String -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len
 = return $ L span $ itint (lexemeToString buf len)
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

-- some conveniences for use with tok_integral
tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral ITinteger

positive :: (Integer -> Integer)
positive = id

negative :: (Integer -> Integer)
negative = negate

decimal :: (Integer, Char -> Int)
decimal = (10, \ c -> ord c - ord '0')

do_bol :: Action
do_bol span _str _len =
  do _ <- popLexState
     lexToken

warnTab :: Action
warnTab srcspan _buf _len =
  do addTabWarning srcspan
     lexToken

data ParseResult a
  = POk PState a
  | PFailed SrcSpan SDoc


data PState = PState {
        buffer     :: StringBuffer,
        tab_first  :: Maybe RealSrcSpan,
        tab_count  :: !Int,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        lex_state  :: [Int],
        annotations :: [(ApiAnnKey,[SrcSpan])],
        comment_q :: [Located AnnotationComment],
        annotations_comments :: [(SrcSpan,[Located AnnotationComment])]
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure a = a `seq` (P $ \s -> POk s a)
  (<*>)  = ap

instance Monad P where
  (P m) >>= k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed span err -> PFailed span err
  fail  = failP

instance MonadFail P where
  fail = failP

failP :: String -> P a
failP msg = P $ \s -> PFailed (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \_ -> PFailed (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg = P $ \_ -> PFailed span msg


getSrcLoc :: P RealSrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

data AlexInput = AI RealSrcLoc StringBuffer

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = fromIntegral $ ord adj_c
        non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'
        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          | otherwise =
                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

addTabWarning :: RealSrcSpan -> P ()
addTabWarning srcspan
 = P $ \s@PState{tab_first=tf, tab_count=tc} ->
       let tf' = if isJust tf then tf else Just srcspan
           tc' = tc + 1
           s'  = s{tab_first = tf', tab_count = tc'}
       in POk s' ()

srcParseErr :: StringBuffer -> Int -> SDoc
srcParseErr buf len
  = if null token
         then text "parse error (possibly incorrect indentation or mismatched brackets)"
         else text "parse error on input" <+> quotes (text token)
  where token = lexemeToString (offsetBytes (-len) buf) len

srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len,
                            last_loc = last_loc } ->
    PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = prevChar buf '\n'

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  (L span tok) <- lexToken
  case tok of
    ITeof -> addAnnotationOnly noSrcSpan AnnEofPos (RealSrcSpan span)
    _ -> return ()

  if (isComment tok)
    then queueComment (L (RealSrcSpan span) tok) >> lexer cont
    else cont (L (RealSrcSpan span) tok)


lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  case alexScanUser 0 inp sc of
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        case unLoc lt of
          ITlineComment _  -> return lt
          ITblockComment _ -> return lt
          lt' -> do
            setLastTk lt'
            return lt

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
     else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)


-- | Encapsulated call to addAnnotation, requiring only the SrcSpan of
--   the AST construct the annotation belongs to; together with the
--   AnnKeywordId, this is is the key of the annotation map
type AddAnn = SrcSpan -> P ()

addAnnotation :: SrcSpan          -- SrcSpan of enclosing AST construct
              -> AnnKeywordId     -- The first two parameters are the key
              -> SrcSpan          -- The location of the keyword itself
              -> P ()
addAnnotation l a v = do
  addAnnotationOnly l a v
  allocateComments l

addAnnotationOnly :: SrcSpan -> AnnKeywordId -> SrcSpan -> P ()
addAnnotationOnly l a v = P $ \s -> POk s {
  annotations = ((l,a), [v]) : annotations s
  } ()

-- |Given a location and a list of AddAnn, apply them all to the location.
addAnnsAt :: SrcSpan -> [AddAnn] -> P ()
addAnnsAt loc anns = mapM_ (\a -> a loc) anns

queueComment :: Located Token -> P()
queueComment c = P $ \s -> POk s {
  comment_q = commentToAnnotation c : comment_q s
  } ()

allocateComments :: SrcSpan -> P ()
allocateComments ss = P $ \s ->
  let
    (before,rest)  = break (\(L l _) -> isSubspanOf l ss) (comment_q s)
    (middle,after) = break (\(L l _) -> not (isSubspanOf l ss)) rest
    comment_q' = before ++ after
    newAnns = if null middle then []
                             else [(ss,middle)]
  in
    POk s {
       comment_q = comment_q'
     , annotations_comments = newAnns ++ (annotations_comments s)
     } ()

commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITlineComment s))     = L l (AnnLineComment s)
commentToAnnotation (L l (ITblockComment s))    = L l (AnnBlockComment s)
commentToAnnotation _                           = error "commentToAnnotation"

isComment :: Token -> Bool
isComment (ITlineComment     _)   = True
isComment (ITblockComment    _)   = True
isComment _ = False


docCommentEnd :: AlexInput -> String -> (String -> Token) -> StringBuffer ->
                 RealSrcSpan -> P (RealLocated Token)
docCommentEnd input commentAcc docType buf span = do
  setInput input
  let (AI loc nextBuf) = input
      comment = reverse commentAcc
      span' = mkRealSrcSpan (realSrcSpanStart span) loc
      last_len = byteDiff buf nextBuf

  span `seq` setLastToken span' last_len
  return (L span' (docType comment))


getPState :: P PState
getPState = P $ \s -> POk s s

-- | Creates a parse state from a 'DynFlags' value
mkPState :: StringBuffer -> RealSrcLoc -> PState
mkPState = mkPStatePure

mkPStatePure :: StringBuffer -> RealSrcLoc -> PState
mkPStatePure buf loc =
  PState {
      buffer        = buf,
      tab_first     = Nothing,
      tab_count     = 0,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      lex_state     = [bol, 0],
      annotations = [],
      comment_q = [],
      annotations_comments = []
    }

type ApiAnnKey = (SrcSpan,AnnKeywordId)

data AnnKeywordId
    = AnnLet
    | AnnIn
    | AnnDcolon -- ^ '::'
    | AnnDcolonU -- ^ '::', unicode variant
    | AnnEqual
    | AnnLam
    | AnnLamU
    | AnnRarrow -- ^ '->'
    | AnnRarrowU -- ^ '->', unicode variant
    | AnnDot    -- ^ '.'
    | AnnOpenC   -- ^ '{'
    | AnnCloseC -- ^ '}'
    | AnnOpenP   -- ^ '('
    | AnnCloseP -- ^ ')'
    | AnnSemi -- ^ ';'
    | AnnComma -- ^ as a list separator
    | AnnName
    | AnnVal  -- ^ e.g. INTEGER
    | AnnEofPos
    | AnnOpen   -- ^ '(\#' or '{-\# LANGUAGE' etc
    | AnnClose -- ^  '\#)' or '\#-}'  etc
    | AnnCommaTuple -- ^ in a RdrName for a tuple
    deriving (Eq, Ord, Data, Show)

data AnnotationComment
  = AnnLineComment     String
  | AnnBlockComment    String
 deriving (Eq, Ord, Data, Show)

data IsUnicodeSyntax
  = UnicodeSyntax
  | NormalSyntax
  deriving (Eq, Ord, Data, Show)

unicodeAnn :: AnnKeywordId -> AnnKeywordId
unicodeAnn AnnLam        = AnnLamU
unicodeAnn AnnDcolon     = AnnDcolonU
unicodeAnn AnnRarrow     = AnnRarrowU
unicodeAnn ann           = ann



bol :: Int
bol = 1
alex_action_1 =  warnTab 
alex_action_2 =  nested_comment lexToken 
alex_action_3 =  lineCommentToken 
alex_action_4 =  lineCommentToken 
alex_action_6 =  do_bol 
alex_action_7 =  special (ITdcolon NormalSyntax)
alex_action_8 =  begin bol 
alex_action_9 =  special IToparen 
alex_action_10 =  special ITcparen 
alex_action_11 =  special ITcomma 
alex_action_12 =  special ITequal 
alex_action_13 =  special (ITlam NormalSyntax) 
alex_action_14 =  special (ITrarrow NormalSyntax) 
alex_action_15 =  special ITdot 
alex_action_16 =  special (ITdcolon UnicodeSyntax) 
alex_action_17 =  special (ITrarrow UnicodeSyntax) 
alex_action_18 =  special (ITlam    UnicodeSyntax) 
alex_action_19 =  special ITlet 
alex_action_20 =  special ITin 
alex_action_21 =  varid 
alex_action_22 =  idtoken conid 
alex_action_23 =  tok_num positive 0 0 decimal 
alex_action_24 =  tok_num negative 1 1 decimal 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/7.10.3/lib/ghc-7.10.3/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 21 "templates/GenericTemplate.hs" #-}

{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 72 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 93 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 105 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
        (AlexNone, input') ->
                case alexGetByte input of
                        Nothing -> 



                                   AlexEOF
                        Just _ ->



                                   AlexError input'

        (AlexLastSkip input'' len, _) ->



                AlexSkip input'' len

        (AlexLastAcc k input''' len, _) ->



                AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
        new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset
                
                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input (len)

        check_accs (AlexAccPred a predx rest)
           | predx user orig_input (len) input
           = AlexLastAcc a input (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user orig_input (len) input
           = AlexLastSkip input (len)
           | otherwise
           = check_accs rest


data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap _ AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap _ (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAccNone
  | AlexAcc a
  | AlexAccSkip

  | AlexAccPred a   (AlexAccPred user) (AlexAcc a user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc a user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
