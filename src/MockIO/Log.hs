{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.Log
  ( DoMock(..), HasDoMock( doMock ), MockIOClass, mkIOL, mkIOL' )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( (&) )
import System.IO      ( IO )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- log-plus ----------------------------

import Log           ( Log, ToDoc_( toDoc_ ), logIO )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊢) )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( parens )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.DoMock        ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )
import MockIO               ( mkIO' )
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

-- that's all, folks! ----------------------------------------------------------
