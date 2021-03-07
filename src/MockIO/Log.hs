{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.Log
  ( DoMock(..), HasDoMock( doMock ), MockIOClass
  , logit, mkIOL, mkIOL', mkIOL'ME, mkIOLME )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Either    ( Either )
import Data.Function  ( (&) )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- exceptions --------------------------

import Control.Monad.Catch ( MonadMask )

-- log-plus ----------------------------

import Log ( CSOpt( NoCallStack ), Log, ToDoc_( toDoc_ ), logIO, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity )

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( IOError )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( parens )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.DoMock        ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )
import MockIO               ( mkIO', mkIO'ME )
import MockIO.IOClass       ( HasIOClass( ioClass ), IOClass )
import MockIO.MockIOClass   ( MockIOClass )

--------------------------------------------------------------------------------

{- | Create an IO action that may be mocked; and log it.

     given:
        -) some logging text (actually, a function from "was it a mock?" to
           logging text (Mocked → Text)
        -) a mock value (ω)
        -) an IO action (IO ω)
     return:
        -) A monad which, when told whether to mock, will (a) act (b) log (c)
           return a value.  The IOClass, and whether the value was actually
           mocked, are annotated in the log.
-}
mkIOL' ∷ ∀ ω τ μ α .
          (MonadIO μ, ToDoc_ τ,
           MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
          Severity     -- ^ log severity
        → IOClass      -- ^ log with this IOClass
        → (DoMock → τ) -- ^ log message, is given {Do,No}Mock so message can
                       -- ^ visually identify whether it really happened
        → IO α         -- ^ mock value; IO is available here so that, e.g., in
                       -- ^ case of mock a file open, /dev/null is opened instead
        → IO α         -- ^ the IO to perform when not mocked
        → DoMock       -- ^ whether to mock
        → μ α
mkIOL' sv ioc lg mock_value io mck = do
  logIO sv (def & ioClass ⊢ ioc & doMock ⊢ mck) (lg mck)
  mkIO' mock_value io mck

--------------------

{- | Mildly simplified `mkIOL'`, specifically with taking constant log message
     (that is surrounded in parens in case of DoMock); and a non-IO mock value.
 -}
mkIOL ∷ ∀ ω τ μ α .
        (MonadIO μ, ToDoc_ τ, 
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → IOClass → τ → α → IO α → DoMock → μ α
mkIOL sv ioc lg mock_value io mck =
  let plog l DoMock = parens (toDoc_ l)
      plog l NoMock = toDoc_ l
   in mkIOL' sv ioc (plog lg) (return mock_value) io mck

----------------------------------------

{- | `mkIOL'`, for MonadError/ExceptT values. -}

mkIOL'ME ∷ ∀ ω τ μ ε α .
            (MonadIO μ, ToDoc_ τ, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity      -- ^ log severity
          → IOClass       -- ^ log with this IOClass
          → (DoMock → τ)  -- ^ log message, is given {Do,No}Mock so message can
                          -- ^ visually identify whether it really happened
          → ExceptT ε μ α -- ^ mock value; IO is available here so that, e.g., in
                          -- ^ case of mock a file open, /dev/null is opened
                          -- ^ instead
          → ExceptT ε μ α -- ^ the IO to perform when not mocked
          → DoMock        -- ^ whether to mock
          → μ α
mkIOL'ME sv ioc lg mock_value io mck = do
  logIO sv (def & ioClass ⊢ ioc & doMock ⊢ mck) (lg mck)
  mkIO'ME mock_value io mck

--------------------

{- | Simplified `mkIOL'ME`. -}
mkIOLME ∷ ∀ ω τ μ α ε .
        (MonadIO μ, ToDoc_ τ, MonadError ε μ,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → IOClass → τ → α → ExceptT ε μ α → DoMock → μ α
mkIOLME sv ioc lg mock_value io mck =
  let plog l DoMock = parens (toDoc_ l)
      plog l NoMock = toDoc_ l
   in mkIOL'ME sv ioc (plog lg) (return mock_value) io mck

----------------------------------------

{- | Log to stderr, no callstack, no transformers: intending for repl
     development & debugging. -}
logit ∷ (MonadIO μ, MonadMask μ) ⇒
        ExceptT IOError (LoggingT (Log MockIOClass) μ) α → μ (Either IOError α)
logit = logToStderr NoCallStack [] ∘ ѥ

-- that's all, folks! ----------------------------------------------------------
