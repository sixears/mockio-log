module MockIO.MockIOClass
  ( MockIOClass( MockIOClass ) )
where

-- base --------------------------------

import Text.Show  ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode  ( (∧) )
import Data.Eq.Unicode    ( (≡) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )

-- tasty-plus --------------------------

import TastyPlus.Equish  ( Equish( (≃) ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.DoMock        ( DoMock( NoMock ), HasDoMock( doMock ) )
import MockIO.IOClass       ( HasIOClass( ioClass ), IOClass )

--------------------------------------------------------------------------------

{- | A unification of IOClass & DoMock for simple mockio logging. -}
data MockIOClass = MockIOClass IOClass DoMock
  deriving Show

instance Default MockIOClass where
  def = MockIOClass def NoMock

instance Equish MockIOClass where
  (MockIOClass i m) ≃ (MockIOClass i' m') = (i ≃ i') ∧ (m ≡ m')

instance HasIOClass MockIOClass where
  ioClass = lens (\ (MockIOClass i _) → i)
                 (\ (MockIOClass _ m) i → MockIOClass i m)

instance HasDoMock MockIOClass where
  doMock = lens (\ (MockIOClass _ m) → m)
                 (\ (MockIOClass i _) m → MockIOClass i m)

-- that's all, folks! ----------------------------------------------------------
