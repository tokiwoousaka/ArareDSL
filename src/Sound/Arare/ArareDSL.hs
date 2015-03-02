{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Arare.ArareDSL where
import Control.Monad.Operational
import Control.Monad.State

--------
-- ArareSequencer : 旋律を記述する

data Note
data SequenceTime

class NoteController n where
  noteOn :: SequenceTime -> Note -> n
  noteOff :: SequenceTime -> Note -> n

class BendController b where
  bendKey :: SequenceTime -> Note -> b

class (NoteController i, BendController i) => Instrument i where

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
-- ArareRhythmBox : リズムパートを記述する

class Percussion p where

data ArareRhythmBoxBase o p a where
  GetPercussionSequenceTime :: ArareRhythmBoxBase o p SequenceTime
  SendParcussionMessage :: p -> ArareRhythmBoxBase o i ()

--------
-- ArareWaveBase : 波形データを直接操作する

data ArareWaveBase where

--------
-- ArareMixer : 各パートをミキシングする

data ArareMixer where


