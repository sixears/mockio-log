module MockIO.Log
  ( DoMock(..), HasDoMock( doMock ), MockIOClass
  , logit, logit', mkIOL, mkIOL', mkIOL'ME, mkIOLME, mkIOLMER )
where

-- base --------------------------------

import Control.Monad  ( forM_, return )
import Data.Function  ( ($), (&) )
import GHC.Stack      ( HasCallStack )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

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

import Data.MoreUnicode.Either  ( 𝔼, pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens    ( (⊢) )
import Data.MoreUnicode.Maybe   ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Text    ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( parens )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

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
            (MonadIO μ, ToDoc_ τ, MonadError ε μ, HasCallStack,
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
        (MonadIO μ, ToDoc_ τ, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → IOClass → τ → α → ExceptT ε μ α → DoMock → μ α
mkIOLME sv ioc lg mock_value io mck =
  let plog l DoMock = parens (toDoc_ l)
      plog l NoMock = toDoc_ l
   in mkIOL'ME sv ioc (plog lg) (return mock_value) io mck

----------------------------------------

{- | Log a mockable IO Action, including its result (if provided a suitable
     formatter), and any exception it throws. -}
mkIOLMER ∷ (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity → IOClass → 𝕋 → 𝕄 (α → [𝕋]) → α
         → ExceptT ε IO α → DoMock → μ α
mkIOLMER sev ioclass msg valmsg mock_value io mck = do
  let stg  = def & ioClass ⊢ ioclass & doMock ⊢ mck
      pp ∷ DoMock → 𝕋 → 𝕋
      pp NoMock t = t
      pp DoMock t = "(" ⊕ t ⊕ ")"
  result ← mkIOL sev ioclass msg (𝕽 mock_value) (ѥ io) mck
  case result of
    𝕷 e → do logIO sev stg (pp mck $ [fmtT|%t FAILED: %T|] msg e)
             throwError e
    𝕽 r → do case valmsg of
               𝕹   → return ()
               𝕵 v → forM_ (v r) $ \ t →
                       logIO sev stg (pp mck $ [fmtT|%t: %t|] msg t)
             return r

----------------------------------------

{- | Log to stderr, no callstack, no transformers: intending for repl
     development & debugging. -}
logit ∷ ∀ ε α μ . (MonadIO μ, MonadMask μ) ⇒
        ExceptT ε (LoggingT (Log MockIOClass) μ) α → μ (𝔼 ε α)
logit = logToStderr NoCallStack [] ∘ ѥ

logit' ∷ ∀ α μ . (MonadIO μ, MonadMask μ) ⇒
         ExceptT IOError (LoggingT (Log MockIOClass) μ) α → μ (𝔼 IOError α)

{- | Like `logit`, but with a fixed error type of `IOError`. -}
logit' = logit

-- that's all, folks! ----------------------------------------------------------
