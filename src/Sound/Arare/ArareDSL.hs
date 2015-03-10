{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.Arare.ArareDSL where
import Control.Monad.Operational
import Control.Monad.State
import Data.Ratio

--------
-- ArareSequencer : トラックを記述する

data SequenceTime

data ArareSequencerBase o i a where
  GetInstrumentSequenceTime :: ArareSequencerBase o i SequenceTime
  PutInstrumentSequenceTime :: SequenceTime -> ArareSequencerBase o i ()
  SendInstrumentMessage :: i -> ArareSequencerBase o i ()

----

type ArareSequencer i = Program (ArareSequencerBase SequenceTime i)

getInstrumentSequenceTime :: ArareSequencer i SequenceTime
getInstrumentSequenceTime = singleton GetInstrumentSequenceTime

putInstrumentSequenceTime :: SequenceTime -> ArareSequencer i ()
putInstrumentSequenceTime = singleton . PutInstrumentSequenceTime

sendInstrumentMessage :: i -> ArareSequencer i ()
sendInstrumentMessage = singleton . SendInstrumentMessage

instance MonadState SequenceTime (ArareSequencer i) where
  get = getInstrumentSequenceTime
  put = putInstrumentSequenceTime  

--------

newtype NoteLength = NoteLength Rational deriving (Show, Read, Eq, Ord, Num)
l1 = NoteLength $ 1 % 1
l2 = NoteLength $ 1 % 2
l4 = NoteLength $ 1 % 4
l8 = NoteLength $ 1 % 8
l16 = NoteLength $ 1 % 16
l32 = NoteLength $ 1 % 32

newtype NoteVelocity = NoteVelocity Float deriving (Show, Read, Eq, Ord)
v = NoteVelocity
vm = v 1
v9 = v 0.9
v8 = v 0.8
v7 = v 0.7
v6 = v 0.6
v5 = v 0.5
v4 = v 0.4
v3 = v 0.3
v2 = v 0.2
v1 = v 0.1
v0 = v 0.0

--------

data NoteScale
  = NoteC
  | NoteCS
  | NoteD
  | NoteDS
  | NoteE
  | NoteF
  | NoteFS
  | NoteG
  | NoteGS
  | NoteA
  | NoteAS
  | NoteB
data Note = Note
  { noteScale :: NoteScale
  , noteLength :: NoteLength
  , noteVelocity :: NoteVelocity
  } 

class NoteController c where
  noteOn :: Note -> c
  noteOff :: Note -> c

class DslNoteController s where
  c :: s
  cs :: s
  d :: s
  ds :: s
  e :: s
  f :: s
  fs :: s
  g :: s
  gs :: s
  a :: s
  as :: s
  b :: s

  df :: s
  df = cs
  ef :: s
  ef = ds
  gf :: s
  gf = fs
  af :: s
  af = gs
  bf :: s
  bf = as

class Monad n => MonadNoteController n where
  getNoteLength :: n NoteLength
  putNoteLength :: NoteLength -> n ()
  getNoteVelocity :: n NoteVelocity
  putNoteVelocity :: NoteVelocity -> n ()

makeNote :: (MonadNoteController (ArareSequencer s), NoteController s)
  => Maybe NoteLength -> Maybe NoteVelocity -> NoteScale -> ArareSequencer s ()
makeNote l v s = let
    judge :: ArareSequencer s a -> Maybe a -> ArareSequencer s a
    judge f v = do 
      x <- f
      return $ maybe x id v
  in do
    nl <- judge getNoteLength l
    nv <- judge getNoteVelocity v
    sendInstrumentMessage . noteOn $ Note s nl nv
    -- TODO : 時間調整
    sendInstrumentMessage . noteOff $ Note s nl nv

infixl 9 .>
(.>) :: Monad m => m () -> m () -> m ()
(.>) = (>>)

--------
-- ArareWaveBase : 波形データを直接操作する

data ArareWaveBase where

--------
-- ArareMixer : 各パートをミキシングする

data ArareMixer where
