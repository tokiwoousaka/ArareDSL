{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.Arare.ArareDSL where
import Control.Monad.Operational
import Control.Monad.State
import Data.Ratio

--------
-- ArareSequencer : トラックを記述する

data SequenceTime

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
  | NoteD
  | NoteE
  | NoteF
  | NoteG
  | NoteA
  | NoteB
data Note = Note
  { noteScale :: NoteScale
  , noteLength :: NoteLength
  , noteVelocity :: NoteVelocity
  } 

makeNote :: Maybe NoteLength -> Maybe NoteVelocity -> NoteScale -> ArareSequencer s ()
makeNote = undefined

class Sequencable s where
  c :: s
  d :: s
  e :: s
  f :: s
  g :: s
  a :: s
  b :: s

infixl 9 .>
(.>) :: Monad m => m () -> m () -> m ()
(.>) = (>>)

--------
-- TODO : ここより下、そのうち削除

data SinOscillator

instance Sequencable (NoteLength -> NoteVelocity -> ArareSequencer SinOscillator ()) where
  c l v = makeNote (Just l) (Just v) NoteC
  d l v = makeNote (Just l) (Just v) NoteD
  e l v = makeNote (Just l) (Just v) NoteE
  f l v = makeNote (Just l) (Just v) NoteF
  g l v = makeNote (Just l) (Just v) NoteG
  a l v = makeNote (Just l) (Just v) NoteA
  b l v = makeNote (Just l) (Just v) NoteB
instance Sequencable (NoteLength -> ArareSequencer SinOscillator ()) where
  c l = makeNote (Just l) Nothing NoteC
  d l = makeNote (Just l) Nothing NoteD
  e l = makeNote (Just l) Nothing NoteE
  f l = makeNote (Just l) Nothing NoteF
  g l = makeNote (Just l) Nothing NoteG
  a l = makeNote (Just l) Nothing NoteA
  b l = makeNote (Just l) Nothing NoteB
instance Sequencable (ArareSequencer SinOscillator ()) where
  c = makeNote Nothing Nothing NoteC
  d = makeNote Nothing Nothing NoteD
  e = makeNote Nothing Nothing NoteE
  f = makeNote Nothing Nothing NoteF
  g = makeNote Nothing Nothing NoteG
  a = makeNote Nothing Nothing NoteA
  b = makeNote Nothing Nothing NoteB

----

test :: ArareSequencer SinOscillator ()
test = do
  -- さ〜い〜た〜さ〜い〜た〜
  c .> d .> e l2 .> c .> d .> e l2
  -- ちゅ〜りっぷ〜の〜は〜な〜が〜
  g .> e .> d .> c .> d .> e .> d l2
