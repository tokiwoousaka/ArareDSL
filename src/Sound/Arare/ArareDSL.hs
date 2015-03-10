{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Arare.ArareDSL where
import Control.Monad.Operational
import Control.Monad.State

--------
-- ArareSequencer : トラックを記述する

class NoteController n where
  noteOn :: Note -> n
  noteOff :: Note -> n

class BendController b where
  bendKey :: Note -> b

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
-- ArareWaveBase : 波形データを直接操作する

data ArareWaveBase where

--------
-- ArareMixer : 各パートをミキシングする

data ArareMixer where

--------
-- Sequencable : シーケンサ本体

data NoteScale
data Note = Note
  { noteScale :: NoteScale
  , noteVelocity :: Float
  } 
data SequenceTime
type NoteLength = Int

--------

class Sequencable s where
  c :: s
  d :: s
  e :: s
  f :: s
  g :: s
  a :: s
  b :: s

--------

data SinOscillator

instance Sequencable (Int -> ArareSequencer SinOscillator a) where
  c = undefined 
  d = undefined 
  e = undefined 
  f = undefined 
  g = undefined 
  a = undefined 
  b = undefined 
instance Sequencable (Int -> Float -> ArareSequencer SinOscillator a) where
  c = undefined 
  d = undefined 
  e = undefined 
  f = undefined 
  g = undefined 
  a = undefined 
  b = undefined 
instance Sequencable (ArareSequencer SinOscillator a) where
  c = undefined 
  d = undefined 
  e = undefined 
  f = undefined 
  g = undefined 
  a = undefined 
  b = undefined 

i2 :: Int
i2 = 2

test :: ArareSequencer SinOscillator ()
test = do
 -- さ〜い〜た〜さ〜い〜た〜
 c >> d >> e i2 >> c >> d >> e i2
 -- ちゅ〜りっぷ〜の〜は〜な〜が〜
 g >> e >> d >> c >> d >> e >> d i2

