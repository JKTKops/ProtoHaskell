{- |
Handles everything to do with messages. Creating, storing, sorting, outputting,
modifying, etc., output, dump, debug, warning, and error messages.

Some terminology:
A 'Message' is a warning or error message. The term 'message' generally refers to
any compiler output that goes to a terminal, but the _type_ is specific to those.

A 'Dump' is a message that contains a representation of some compiler state, like an AST
or the state of the renamer's environment.

An info message (TODO: InfoMessage type?) is simply a message containing some form of
information the user asked for, but isn't a warning or error. Verbosity messages are info
messages.
-}
module Compiler.Messages where

import Compiler.Settings
import Compiler.BasicTypes.SrcLoc

import Utils.Outputable

import Data.Bag

newtype Messages = Messages
    { getMessages :: ( WarningMessages
                     , ErrorMessages
                     )
    }
type WarningMessages = Bag WarningMessage
type ErrorMessages   = Bag ErrorMessage

unionMessages :: Messages -> Messages -> Messages
unionMessages (Messages (w1, e1)) (Messages (w2, e2)) =
    Messages (w1 <> w2, e1 <> e2)

emptyMessages :: Messages
emptyMessages = Messages (emptyBag, emptyBag)

instance Semigroup Messages where
    (<>) = unionMessages
instance Monoid Messages where
    mempty = emptyMessages

data Message = Message
    { mSpan        :: SrcSpan
    , mDoc         :: MessageDoc
    , mShortString :: String
    , mSeverity    :: Severity
    , mReason      :: MessageReason
    }
type ErrorMessage   = Message
type WarningMessage = Message

-- unlike ghc, possible fix probably belongs in its own section
data MessageDoc = MessageDoc
    { mdImportant     :: CDoc
        -- ^ The actual error message, e.g
        -- "Couldn't match expected type `Bool' with actual type `Int'"
    , mdContext       :: [CDoc]
        -- ^ Location context, e.g. "In the first argument of ..."
    , mdSupplementary :: [CDoc]
        -- ^ Supplementary information, e.g. "Relevant bindings include ..."
    }

-- | Reasons associate with messages containing warnings.
-- The message may be something that we always print,
-- it may have been enabled by a flag, or it may have been turned into an
-- error by a flag.
data MessageReason
     = NoReason
     | WarnReason  WarnFlag
     | ErrorReason WarnFlag

-- | Possible severities of messages. Used modestly to put headers
-- on messages and (TODO) for coloring.
-- Pretty much a copy of GHC's corresponding type.
data Severity
     = SevOutput
     | SevFatal
     | SevInteractive
     | SevDump -- ^ intended for compiler development
     | SevInfo -- ^ intended for end users

     | SevWarning
     | SevError
     deriving (Eq, Show)

formatMessageDoc :: MessageDoc -> CDoc
formatMessageDoc MessageDoc
  {mdImportant = important, mdContext = context, mdSupplementary = supl} =
    vcat $ map (bullet <+>) components
  where
    separatedComponents = [important] : filter (not . null) [context, supl]
    components = map vcat separatedComponents

formatMessage :: Message -> CDoc
formatMessage Message{mSpan, mDoc, mSeverity, mReason} =
    withCDocSettings $ \s ->
        setCDocStyle (mkMessageStyle s) $
            (ppr (srcSpanStart mSpan) <> colon <+> annotation <+> formatReason mReason) $$
            nest 4 (formatMessageDoc mDoc)
  where
    formatReason :: MessageReason -> CDoc
    formatReason NoReason = empty
    -- TODO: We want to ppr the flags, not show them, but flg ppr isn't written yet
    formatReason (WarnReason flg)  = brackets $ ppr (show flg)
    formatReason (ErrorReason flg) = brackets $ ppr (show flg)

    annotation = case mSeverity of
        SevWarning -> text "warning" <> colon
        SevError   -> text "error"   <> colon
        _          -> empty
