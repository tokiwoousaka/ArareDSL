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
  ArareFromScore :: Score () -> ArareSequencerBase o i ()

----

type ArareSequencer i = Program (ArareSequencerBase SequenceTime i)

getInstrumentSequenceTime :: ArareSequencer i SequenceTime
getInstrumentSequenceTime = singleton GetInstrumentSequenceTime

putInstrumentSequenceTime :: SequenceTime -> ArareSequencer i ()
putInstrumentSequenceTime = singleton . PutInstrumentSequenceTime

sendInstrumentMessage :: i -> ArareSequencer i ()
sendInstrumentMessage = singleton . SendInstrumentMessage

score :: Score () -> ArareSequencer i ()
score = singleton . ArareFromScore

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

data ScoreBase a where
  WriteNote :: Note -> NoteLength -> ScoreBase ()
  GetSequenceTime :: ScoreBase SequenceTime
  PutSequenceTime :: SequenceTime -> ScoreBase ()

type Score = Program ScoreBase 

writeNote :: Note -> NoteLength -> Score ()
writeNote n = singleton . WriteNote n

getSequenceTime :: Score SequenceTime
getSequenceTime = singleton $ GetSequenceTime

petSequenceTime :: SequenceTime -> Score ()
petSequenceTime = singleton . PutSequenceTime

--------

c :: Int -> Float -> Score ()
c = undefined

d :: Int -> Float -> Score ()
d = undefined

e :: Int -> Float -> Score ()
e = undefined

f :: Int -> Float -> Score ()
f = undefined

g :: Int -> Float -> Score ()
g = undefined

--------

class Sequencable s where

instance Sequencable (Float -> Score ()) where
instance Sequencable (Int -> Float -> Score ()) where
instance Sequencable (Score ()) where

infixl 9 ==>
(==>) :: (Sequencable a, Sequencable b) => a -> b -> Score ()
(==>) = undefined

--------
-- test code

data SinOscillator

test :: ArareSequencer i ()
test = do
  score $ do 
   -- さ〜い〜た〜さ〜い〜た〜
   c ==> d ==> e 2 ==> c ==> d ==> e 2
   -- ちゅ〜りっぷ〜の〜は〜な〜が〜
   g ==> e ==> d ==> c ==> d ==> e ==> d 2

