{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.T.Log
  ( tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($), const )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import GHC.Exts       ( fromList )
import GHC.Stack      ( SrcLoc( SrcLoc ) )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- data-default ------------------------

import Data.Default  ( Default )

-- log-plus ----------------------------

import Log           ( CSOpt( FullCallStack ), Log, logIOT, logToStderr )
import Log.LogEntry  ( LogEntry, logEntry )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational )
                          , runPureLoggingT )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( pretty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus         ( (≟), runTestsP, runTestsReplay
                         , runTestTree,withResource', withResource2' )
import TastyPlus.Equish  ( Equish( (≃) ) )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( putStrLn, readFile )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime, getCurrentTime )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Log           ( mkIOL' )
import MockIO.DoMock        ( DoMock( DoMock, NoMock ), HasDoMock )
import MockIO.IOClass       ( HasIOClass, IOClass( IORead ) )
import MockIO.MockIOClass   ( MockIOClass( MockIOClass ) )


--------------------------------------------------------------------------------

_li0 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li0 = logIOT Informational "li0" ⪼ return "Godzilla"

_li1 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li1 = do
  _ ← _li0
  logIOT Informational "li1"
  return "MUTO"

------------------------------------------------------------

{- This exists here for testing only; real file reading/writing will be in a
   different package. -}
readFn ∷ ∀ ω μ . (MonadIO μ,
                  MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         String → DoMock → μ Text
readFn s = mkIOL' Informational IORead (const $ [fmtT|read %s|] s)
                                       (return "mock text") $
           readFile s

------------------------------------------------------------

readFnTests ∷ TestTree
readFnTests =
  let src_loc     = SrcLoc "main" "MockIO.Log" "src/MockIO/Log.hs" 130 3 138 58
      call_list   = [("logIO"∷String,src_loc)]
      log_entry   ∷ UTCTime → LogEntry MockIOClass
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group")
                             (MockIOClass IORead NoMock)

      log_ ∷ UTCTime → Log MockIOClass
      log_ t       = fromList [ log_entry t ]

      log_string ∷ UTCTime → Log MockIOClass → String
      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource2' (runPureLoggingT $ readFn @MockIOClass "/etc/group" NoMock)
                     (readFile "/etc/group") $ \ txtlog exptxt →
        testGroup "readFn"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← exptxt
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        t ← getCurrentTime
                                        assertBool (log_string t lg)
                                                   (log_ t ≃ lg)
                  ]

readFnMockTests ∷ TestTree
readFnMockTests =
  let src_loc     = SrcLoc "main" "MockIO.Log" "src/MockIO/Log.hs" 130 3 130 57
      call_list   = [("logIO"∷String,src_loc)]
      log_entry   ∷ UTCTime → LogEntry MockIOClass
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group")
                             (MockIOClass IORead DoMock)

      log_ ∷ UTCTime → Log MockIOClass
      log_ t       = fromList [ log_entry t ]

      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource' (runPureLoggingT $ readFn "/etc/group" DoMock) $
                    \ txtlog  →
        testGroup "readFn-Mock"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← return "mock text"
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        t ← getCurrentTime
                                        assertBool (log_string t lg)
                                                   (log_ t ≃ lg)
                  ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ readFnTests, readFnMockTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

_testm ∷ IO ()
_testm = do
  gr ← logToStderr FullCallStack [] $ readFn @MockIOClass "/etc/group" NoMock
  putStrLn "---- /etc/group"
  putStrLn gr
  putStrLn "---------------"

-- that's all, folks! ----------------------------------------------------------
