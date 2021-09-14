module MockIO.IOClass
  ( HasIOClass( ioClass ), IOClass(..), IOClassSet
  , (∈), ioClasses, ioClassParses, isExternalIO, isInternalIO, member )
where

import GHC.Exts  ( IsList( Item, fromList, toList ) )

-- base --------------------------------

import Control.Monad       ( return )
import Data.Bool           ( Bool( False, True ), not )
import Data.Eq             ( Eq )
import Data.Function       ( ($), id )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Ord            ( Ord )
import Data.String         ( String )
import GHC.Enum            ( Enum )
import GHC.Generics        ( Generic )
import System.Exit         ( ExitCode )
import System.IO           ( IO )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import qualified Data.Set  as  Set

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( MemberType, member, (∈) ) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens  ( Lens' )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫) )
import Data.MoreUnicode.Bool         ( 𝔹 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Natural      ( ℕ )

-- parsec ------------------------------

import Text.Parsec.Char        ( char )
import Text.Parsec.Combinator  ( sepBy )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parsec, parser ), ParseError )

-- parser-plus -------------------------

import ParserPlus  ( caseInsensitiveString, tries )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus         ( assertRight, runTestsP, runTestTree, runTestsReplay )
import TastyPlus.Equish  ( Equish( (≃) ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data IOClass = IORead  -- ^ An IO action that perceives but does not alter state
                       --   (e.g., read a file, or the system clock).
             | IOWrite -- ^ An IO action that may alter state
                       --   (e.g., write a file, or to the network).
             | IOCmdR  -- ^ An external cmd (results in an execve or fork call)
                       --   that perceives but does not alter state.
             | IOCmdW  -- ^ An external cmd (results in an execve or fork call)
                       --   that may alter state.
             | IOExec  -- ^ An exec (replaces this executable).
             | NoIO    -- ^ No IO.
  -- ordering is not relevant, we just derive it to support Set
  deriving (Enum,Eq,Generic,NFData,Ord,Show)

--------------------

{- | Lookup table of IOClass to possible (case-insensitive) string
     representations. -}
ioClassParses ∷ IOClass → [String]
ioClassParses IORead  = [ "IORead",     "IOR" ]
ioClassParses IOWrite = [ "IOWrite",    "IOW" ]
ioClassParses IOCmdR  = [ "IOCmdRead",  "IOCmdR" ]
ioClassParses IOCmdW  = [ "IOCmdWrite", "IOCmdW" ]
ioClassParses IOExec  = [ "IOExec" ]
ioClassParses NoIO    = [ "NoIO" ]

instance Parsecable IOClass where
  parser =
    let strs =    ("IORead"     , IORead)
             :| [ ("IOR"        , IORead)
                , ("IOWrite"    , IOWrite)
                , ("IOW"        , IOWrite)
                , ("IOCmdRead"  , IOCmdR)
                , ("IOCmdR"     , IOCmdR)
                , ("IOCmdWrite" , IOCmdW)
                , ("IOCmdW"     , IOCmdW)
                , ("IOExec"     , IOExec)
                , ("NoIO"       , NoIO)
                ]
     in tries [ caseInsensitiveString @_ @[] st ⋫ return ioc | (st,ioc) ← strs]

----------

instance Default IOClass where
  def = NoIO

----------

instance Printable IOClass where
  print = P.string ∘ show

----------

instance Equish IOClass where
  i ≃ i' = i ≡ i'

--------------------

class HasIOClass α where
  ioClass ∷ Lens' α IOClass

----------

instance HasIOClass IOClass where
  ioClass = id

----------------------------------------

{-| Predicate for IO that outside of this process (utilizes exec*); that is,
    exclude `NoIO`, `IORead` & `IOWrite`; leaving `IOCmdR`, `IOCmdW`, `IOExec`.
 -}
isExternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isExternalIO a = case a ⊣ ioClass of
                   NoIO    → False
                   IORead  → False
                   IOWrite → False
                   IOCmdR  → True
                   IOCmdW  → True
                   IOExec  → True

----------------------------------------

{-| Logical inverse of `isExternalIO`. -}
isInternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isInternalIO = not ∘ isExternalIO

------------------------------------------------------------

newtype IOClassSet = IOClassSet { unIOClassSet ∷ Set.Set IOClass }
  deriving (Eq, Show)

instance HasMember IOClassSet where
  type MemberType IOClassSet = IOClass
  member ∷ IOClass → IOClassSet → 𝔹
  member ioc (IOClassSet iocs) = ioc `Set.member` iocs

----------

instance IsList IOClassSet where
  -- requirement is that fromList ∘ toList = id (not the other way round)
  type Item IOClassSet = IOClass
  fromList = IOClassSet ∘ Set.fromList
  toList   = Set.toList ∘ unIOClassSet

----------

instance Printable IOClassSet where
  print (toList → iocs) = P.text $ [fmt|«%L»|] iocs

----------

instance Parsecable IOClassSet where
  parser = fromList ⊳ (parser `sepBy` (char ','))

parseIOClassSetTests ∷ TestTree
parseIOClassSetTests =
  let test ∷ String → IOClassSet → TestTree
      test txt exp =
        testCase txt $
          assertRight (@=? exp) (parsec @IOClassSet @ParseError txt txt)
   in testGroup "Parsecable"
                [ test "iocmdw" (fromList [IOCmdW])
                , test "iocmdw,iocmdr" (fromList [IOCmdW, IOCmdR])
                , test "ioread,iow" (fromList [IORead, IOWrite])
                ]

----------------------------------------

ioClasses ∷ IOClassSet
ioClasses =
  IOClassSet $ Set.fromList [ IORead, IOWrite, IOCmdR, IOCmdW, IOExec, NoIO ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "IOClass" [ parseIOClassSetTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
