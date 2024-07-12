{-# LANGUAGE UnicodeSyntax #-}
module MockIO.Log
  ( DoMock(..)
  , HasDoMock(doMock)
  , MockIOClass
  , alertIO
  , alertIO'
  , criticalIO
  , criticalIO'
  , debugIO
  , debugIO'
  , emergencyIO
  , emergencyIO'
  , errIO
  , errIO'
  , infoIO
  , infoIO'
  , logResult
  , logio
  , logit
  , logit'
  , mkIOL
  , mkIOL'
  , mkIOL'ME
  , mkIOLME
  , mkIOLMER
  , noticeIO
  , noticeIO'
  , warnIO
  , warnIO'
  ) where

import Base1T

-- base --------------------------------

import Data.Foldable ( Foldable )

-- exceptions --------------------------

import Control.Monad.Catch ( MonadMask )

-- log-plus ----------------------------

import Log ( CSOpt(NoCallStack), Log, ToDoc_(toDoc_), logIO, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog,
                           Severity(Alert, Critical, Debug, Emergency, Error, Informational, Notice, Warning) )

-- monadio-error -----------------------

import MonadError.IO.Error ( IOError )

-- prettyprinter -----------------------

import Prettyprinter ( parens )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO             ( mkIO', mkIO'ME' )
import MockIO.DoMock      ( DoMock(DoMock, NoMock), HasDoMock(doMock) )
import MockIO.IOClass     ( HasIOClass(ioClass), IOClass(Except) )
import MockIO.MockIOClass ( MockIOClass )

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
            (μ (𝔼 ε α) → μ (𝔼 ε α)) -- ^ handle / log / amend return value
          → Severity                 -- ^ log severity
          → IOClass                  -- ^ log with this IOClass
          → (DoMock → τ)             -- ^ log message, is given {Do,No}Mock so
                                     --   message can visually identify whether
                                     --   it really happened
          → ExceptT ε μ α            -- ^ mock value; IO is available here so
                                     --   that, e.g., in case of mock a file
                                     --   open, /dev/null is opened instead
          → ExceptT ε μ α            -- ^ the IO to perform when not mocked
          → DoMock                   -- ^ whether to mock
          → μ α
mkIOL'ME handle sv ioc lg mock_value io mck = do
  logIO sv (def & ioClass ⊢ ioc & doMock ⊢ mck) (lg mck)
  mkIO'ME' handle mock_value io mck

--------------------

{- | Simplified `mkIOL'ME`. -}
mkIOLME ∷ ∀ ω τ μ α ε .
        (MonadIO μ, ToDoc_ τ, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → IOClass → τ → α → ExceptT ε μ α → DoMock → μ α
mkIOLME sv ioc lg mock_value io mck =
  let plog l DoMock = parens (toDoc_ l)
      plog l NoMock = toDoc_ l
      whenLeft h x = case x of 𝕷 e → h e; 𝕽 _ → return ()
      logExcept e = logIO sv (def & ioClass ⊢ Except) (toText e)
      do' ∷ Monad η ⇒ (β → η ()) → η β → η β
      do' f r = do { r ≫ f; r }
   in mkIOL'ME (do' (whenLeft logExcept))
               sv ioc (plog lg) (return mock_value) io mck

----------------------------------------

{-| Surround some text with parens, if mocked. -}
pp ∷ DoMock → 𝕋 → 𝕋
pp NoMock t = t
pp DoMock t = "(" ⊕ t ⊕ ")"

----------------------------------------

{-| Log the result of an IO Action. -}
logResult ∷ (MonadIO μ, MonadLog (Log ω) μ, Printable ε, MonadError ε μ,
             Foldable φ) ⇒
            Severity → ω → DoMock → 𝕋 → 𝕄 (α → φ 𝕋) → 𝔼 ε α → μ α
logResult _   _ _   _   𝕹     (𝕽 r) = return r
logResult sev w mck msg _     (𝕷 e) =
  logIO sev w (pp mck $ [fmtT|%t FAILED: %T|] msg e) ⪼ throwError e
logResult sev w mck msg (𝕵 v) (𝕽 r) =
  forM_ (v r) (\ t → logIO sev w (pp mck $ [fmtT|%t: %t|] msg t)) ⪼ return r

----------------------------------------

{- | Log a mockable IO Action, including its result (if provided a suitable
     formatter), and any exception it throws. -}
mkIOLMER ∷ (MonadIO μ, Printable ε, MonadError ε μ, HasCallStack,
            MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
           Severity → IOClass → 𝕋 → 𝕄 (α → [𝕋]) → α → ExceptT ε IO α → DoMock
         → μ α
mkIOLMER sev ioclass msg valmsg mock_value io mck = do
  result ← mkIOL sev ioclass msg (𝕽 mock_value) (ѥ io) mck
  logResult sev (def & ioClass ⊢ ioclass & doMock ⊢ mck) mck msg valmsg result

----------------------------------------

{- | Log a message at a given level with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
logio ∷ ∀ ρ ω μ . (MonadIO μ, ToDoc_ ρ,
                   Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
        Severity → ρ → DoMock → μ ()
logio sev msg do_mock = mkIOL sev def msg () (return ()) do_mock

{- | Log a message, at Emergency level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
emergencyIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒
              DoMock → ρ → μ ()
emergencyIO do_mock msg = logio Emergency msg do_mock

{- | Like `emergencyIO`, but with `Text` and `NoMock`. -}
emergencyIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
emergencyIO' = emergencyIO NoMock

----------------------------------------

{- | Log a message, at Alert level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
alertIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒ DoMock → ρ → μ()
alertIO do_mock msg = logio Alert msg do_mock

{- | Like `alertIO`, but with `Text` and `NoMock`. -}
alertIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
alertIO' = alertIO NoMock

----------------------------------------

{- | Log a message, at Critical level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
criticalIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒
             DoMock → ρ → μ ()
criticalIO do_mock msg = logio Critical msg do_mock

{- | Like `criticalIO`, but with `Text` and `NoMock`. -}
criticalIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
criticalIO' = criticalIO NoMock

----------------------------------------

{- | Log a message, at Error level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
errIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒ DoMock → ρ → μ ()
errIO do_mock msg = logio Error msg do_mock

{- | Like `errIO`, but with `Text` and `NoMock`. -}
errIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
errIO' = errIO NoMock

----------------------------------------

{- | Log a message, at Warn level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
warnIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒ DoMock → ρ → μ ()
warnIO do_mock msg = logio Warning msg do_mock

{- | Like `warnIO`, but with `Text` and `NoMock`. -}
warnIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
warnIO' = warnIO NoMock

----------------------------------------

{- | Log a message, at Notice level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
noticeIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒
           DoMock → ρ → μ ()
noticeIO do_mock msg = logio Notice msg do_mock

{- | Like `noticeIO`, but with `Text` and `NoMock`. -}
noticeIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
noticeIO' = noticeIO NoMock

----------------------------------------

{- | Log a message, at Info level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
infoIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒
           DoMock → ρ → μ ()
infoIO do_mock msg = logio Informational msg do_mock

{- | Like `infoIO`, but with `Text` and `NoMock`. -}
infoIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
infoIO' = infoIO NoMock

----------------------------------------

{- | Log a message, at Debug level, with MockIOClass defaults; use IO so the
     message gets a timestamp. -}
debugIO ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ, ToDoc_ ρ) ⇒
           DoMock → ρ → μ ()
debugIO do_mock msg = logio Debug msg do_mock

{- | Like `debugIO`, but with `Text` and `NoMock`. -}
debugIO' ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ) ⇒ 𝕋 → μ ()
debugIO' = debugIO NoMock

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
