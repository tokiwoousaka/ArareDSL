{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Arare.ArareDSL where
import Control.Monad.Operational
import Control.Monad.State

--------
-- ArareSequencer : 旋律を記述する

data SequenceTime

data ArareSequencerBase i a where
  GetInstrumentSequenceTime :: ArareSequencerBase i SequenceTime
  PutInstrumentSequenceTime :: SequenceTime -> ArareSequencerBase i ()
  SendInstrumentMessage :: i -> ArareSequencerBase i ()

----

type ArareSequencer i = Program (ArareSequencerBase i)

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

data NoteScale 
  = NoteC 
  | NoteD 
  | NoteE 
  | NoteF 
  | NoteG 
  | NoteA 
  | NoteB 
  deriving (Show, Read, Eq, Ord)
data Note = Note
  { noteScale :: NoteScale
  , noteLength :: Int 
  , noteVelocity :: Float
  }

class NoteController n where
  noteOn :: Note -> n

class BendController b where
  bendKey :: Float -> b

class (NoteController i, BendController i) => Instrument i where

sendNoteMessage :: NoteController n => NoteScale -> Int -> Float -> ArareSequencer n ()
sendNoteMessage n l v = sendInstrumentMessage . noteOn $ Note
  { noteScale = n
  , noteLength = l
  , noteVelocity = v
  }

c :: NoteController n => Int -> Float -> ArareSequencer n ()
c = sendNoteMessage NoteC

d :: NoteController n => Int -> Float -> ArareSequencer n ()
d = sendNoteMessage NoteD

e :: NoteController n => Int -> Float -> ArareSequencer n ()
e = sendNoteMessage NoteE

f :: NoteController n => Int -> Float -> ArareSequencer n ()
f = sendNoteMessage NoteF

--------

class Sequensable n s where

instance NoteController n => Sequensable n (Int -> Float -> ArareSequencer n ()) where
instance NoteController n => Sequensable n (Float -> ArareSequencer n ()) where
instance NoteController n => Sequensable n (ArareSequencer n ()) where

infixr 9 <>
(<>) :: (NoteController n, Sequensable n a, Sequensable n b) => a -> b -> ArareSequencer n ()
(<>) = undefined

--------

data SinOscillator = SinOscillator
instance NoteController SinOscillator where
  noteOn _ = SinOscillator

test :: ArareSequencer SinOscillator ()
test = c <> d

--------
-- ArareRhythmBox : リズムパートを記述する

class Percussion p where

data ArareRhythmBoxBase p a where
  GetPercussionSequenceTime :: ArareRhythmBoxBase p SequenceTime
  PutPercussionSequenceTime :: SequenceTime -> ArareRhythmBoxBase p a
  SendPercussionMessage :: p -> ArareRhythmBoxBase i ()

----

type ArareRhythmBox p = Program (ArareRhythmBoxBase p)

getPercussionSequenceTime :: ArareRhythmBox i SequenceTime
getPercussionSequenceTime = singleton GetPercussionSequenceTime

putPercussionSequenceTime :: SequenceTime -> ArareRhythmBox i ()
putPercussionSequenceTime = singleton . PutPercussionSequenceTime

sendPercussionMessage :: i -> ArareRhythmBox i ()
sendPercussionMessage = singleton . SendPercussionMessage

instance MonadState SequenceTime (ArareRhythmBox i) where
  get = getPercussionSequenceTime
  put = putPercussionSequenceTime

--------
-- ArareWaveBase : 波形データを直接操作する

data ArareWaveBase where

--------
-- ArareMixer : 各パートをミキシングする

data ArareMixer where


