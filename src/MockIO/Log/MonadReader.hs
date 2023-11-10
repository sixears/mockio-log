{-# LANGUAGE UnicodeSyntax #-}

{-| Logging functions that use MonadReader to get DoMock from the calling
    environment -}

module MockIO.Log.MonadReader
  ( alert
  , critical
  , debug
  , emergency
  , err
  , info
  , notice
  , warn
  ) where

import Base1T

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- log-plus ----------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog )

-- mockio ------------------------------

import MockIO.DoMock ( HasDoMock(doMock) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, asks )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Log ( MockIOClass, alertIO, criticalIO, debugIO, emergencyIO,
                    errIO, infoIO, noticeIO, warnIO )

--------------------------------------------------------------------------------

emergency ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
emergency t = asks (view doMock) ≫ \ mock → emergencyIO mock t

alert ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
alert t = asks (view doMock) ≫ \ mock → alertIO mock t

critical ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
critical t = asks (view doMock) ≫ \ mock → criticalIO mock t

err ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
err t = asks (view doMock) ≫ \ mock → errIO mock t

warn ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
warn t = asks (view doMock) ≫ \ mock → warnIO mock t

notice ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                 MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
notice t = asks (view doMock) ≫ \ mock → noticeIO mock t

info ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
info t = asks (view doMock) ≫ \ mock → infoIO mock t

debug ∷ ∀ δ η . (MonadReader δ η, HasDoMock δ, MonadIO η,
                  MonadLog (Log MockIOClass) η) ⇒ 𝕋 → η ()
debug t = asks (view doMock) ≫ \ mock → debugIO mock t

-- that's all, folks! ----------------------------------------------------------
