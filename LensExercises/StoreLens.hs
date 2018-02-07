module StoreLens (
  Store(..)
, setS
, getS
, mapS
, duplicateS
, extendS
, extractS
, Lens(..)
, getsetLaw
, setgetLaw
, setsetLaw
, get
, set
, modify
, (%~)
, fmodify
, (|=)
, fstL
, sndL
, mapL
, setL
, compose
, (|.)
, identity
, product
, (***)
, choice
, (|||)
, cityL
, countryL
, streetL
, suburbL
, localityL
, ageL
, nameL
, addressL
, getSuburb
, setStreet
, getAgeAndCountry
, setCityAndLocality
, getSuburbOrCity
, setStreetOrState
, modifyCityUppercase
) where

import Control.Applicative(Applicative((<*>)))
import Data.Char(toUpper)
import Data.Functor
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)
import First(Store(Store), Person(Person), Locality(Locality), Address(Address), bool)
import Prelude hiding (product)
import Control.Comonad


setS :: Store s a -> s -> a
setS (Store s _) = s

getS :: Store s a -> s
getS (Store _ g) = g

mapS :: (a -> b) -> Store s a -> Store s b
mapS f (Store g x) = Store (f . g) x

duplicateS :: Store s a -> Store s (Store s a)
duplicateS x = Store (const x) (getS x)

extendS :: (Store s a -> b) -> Store s a -> Store s b
extendS f (Store f' x') = Store (f . (Store f')) x'

extractS :: Store s a -> a
extractS (Store g x) = g x

instance Functor (Store s) where
  fmap = mapS

instance Comonad (Store s) where
  extend = extendS
  extract = extractS

----

data Lens a b = Lens (a -> Store b a)

get :: Lens a b -> a -> b
get (Lens r) = getS . r

set :: Lens a b -> a -> b -> a
set (Lens r) = setS . r

getsetLaw :: Eq a => Lens a b -> a -> Bool
getsetLaw l = \a -> set l a (get l a) == a

setgetLaw :: Eq b => Lens a b -> a -> b -> Bool
setgetLaw l a b = get l (set l a b) == b

setsetLaw :: Eq a => Lens a b -> a -> b -> b -> Bool
setsetLaw l a b1 b2 = set l (set l a b1) b2 == set l a b2

modify :: Lens a b -> (b -> b) -> a -> a
modify len f target = set len target (f (get len target))

(%~) :: Lens a b -> (b -> b) -> a -> a
(%~) = modify

infixr 4 %~

(.~) :: Lens a b -> b -> a -> a
(.~) = (\len -> flip (set len))

infixl 5 .~

fmodify :: Functor f => Lens a b -> (b -> f b) -> a -> f a
fmodify lens f target = f ((get lens) target) $> target

infixl 5 |=

(|=) :: Functor f => Lens a b -> f b -> a -> f a
(|=) lens val target = fmodify lens (const val) target

fstL :: Lens (x, y) x
fstL = Lens (\p -> Store (\ x -> (x, snd p)) (fst p))


sndL :: Lens (x, y) y
sndL = Lens (\p -> Store (\ y -> (fst p, y)) (snd p))

mapL :: Ord k => k -> Lens (Map k v) (Maybe v)
mapL key = Lens (\ kv -> Store ( \val -> case val of
    Nothing -> Map.delete key kv
    Just x -> Map.insert key x kv
    ) (Map.lookup key kv))

setL :: Ord k => k -> Lens (Set k) Bool
setL key = Lens (\set -> Store (\b -> case b of
    False -> Set.delete key set
    True -> Set.insert key set
  ) (Set.member key set))

compose :: Lens b c -> Lens a b -> Lens a c
compose f g = Lens (\x -> Store (((set g) x) . (set f) (get g x)) ((get f . get g) $ x))

(|.) :: Lens b c -> Lens a b -> Lens a c
(|.) = compose

infixr 9 |.

identity :: Lens a a
identity = Lens (\x -> Store id x)

product :: Lens a b -> Lens c d -> Lens (a, c) (b, d)
product l1 l2 = undefined

(***) :: Lens a b -> Lens c d -> Lens (a, c) (b, d)
(***) = product

infixr 3 ***

-- |
--
-- >>> get (choice fstL sndL) (Left ("abc", 7))
-- "abc"
--
-- >>> get (choice fstL sndL) (Right ("abc", 7))
-- 7
--
-- >>> set (choice fstL sndL) (Left ("abc", 7)) "def"
-- Left ("def",7)
--
-- >>> set (choice fstL sndL) (Right ("abc", 7)) 8
-- Right ("abc",8)
choice :: Lens a x -> Lens b x -> Lens (Either a b) x
choice = error "todo: choice"

-- | An alias for @choice@.
(|||) :: Lens a x -> Lens b x -> Lens (Either a b) x
(|||) = choice

infixr 2 |||

----

cityL :: Lens Locality String
cityL = Lens (\(Locality c t y) -> Store (\c' -> Locality c' t y) c)

stateL :: Lens Locality String
stateL = Lens (\(Locality c t y) -> Store (\t' -> Locality c t' y) t)

countryL :: Lens Locality String
countryL = Lens (\(Locality c t y) -> Store (\y' -> Locality c t y') y)

streetL :: Lens Address String
streetL = Lens (\(Address t s l) -> Store (\t' -> Address t' s l) t)

suburbL :: Lens Address String
suburbL = Lens (\(Address t s l) -> Store (\s' -> Address t s' l) s)

localityL :: Lens Address Locality
localityL = Lens (\(Address t s l) -> Store (\l' -> Address t s l') l)

ageL ::
  Lens Person Int
ageL =
  Lens
    (\(Person a n d) ->
      Store (\a' -> Person a' n d) a)

nameL ::
  Lens Person String
nameL =
  Lens
    (\(Person a n d) ->
      Store (\n' -> Person a n' d) n)

addressL ::
  Lens Person Address
addressL =
  Lens
    (\(Person a n d) ->
    Store (\d' -> Person a n d') d)

-- |
--
-- >>> getSuburb fred
-- "Fredville"
--
-- >>> getSuburb mary
-- "Maryland"
getSuburb ::
  Person
  -> String
getSuburb =
  error "todo: getSuburb"

-- |
--
-- >>> setStreet fred "Some Other St"
-- Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setStreet mary "Some Other St"
-- Person 28 "Mary" (Address "Some Other St" "Maryland" (Locality "Mary Mary" "Western Mary" "Maristan"))
setStreet ::
  Person
  -> String
  -> Person
setStreet =
  error "todo: setStreet"

-- |
--
-- >>> getAgeAndCountry (fred, maryLocality)
-- (24,"Maristan")
--
-- >>> getAgeAndCountry (mary, fredLocality)
-- (28,"Fredalia")
getAgeAndCountry ::
  (Person, Locality)
  -> (Int, String)
getAgeAndCountry =
  error "todo: getAgeAndCountry"

-- |
--
-- >>> setCityAndLocality (fred, maryAddress) ("Some Other City", fredLocality)
-- (Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "Some Other City" "New South Fred" "Fredalia")),Address "83 Mary Ln" "Maryland" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setCityAndLocality (mary, fredAddress) ("Some Other City", maryLocality)
-- (Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "Some Other City" "Western Mary" "Maristan")),Address "15 Fred St" "Fredville" (Locality "Mary Mary" "Western Mary" "Maristan"))
setCityAndLocality ::
  (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality =
  error "todo: setCityAndLocality"

-- |
--
-- >>> getSuburbOrCity (Left maryAddress)
-- "Maryland"
--
-- >>> getSuburbOrCity (Right fredLocality)
-- "Fredmania"
getSuburbOrCity ::
  Either Address Locality
  -> String
getSuburbOrCity =
  error "todo: getSuburbOrCity"

-- |
--
-- >>> setStreetOrState (Right maryLocality) "Some Other State"
-- Right (Locality "Mary Mary" "Some Other State" "Maristan")
--
-- >>> setStreetOrState (Left fred) "Some Other St"
-- Left (Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia")))
setStreetOrState ::
  Either Person Locality
  -> String
  -> Either Person Locality
setStreetOrState =
  error "todo: setStreetOrState"

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase ::
  Person
  -> Person
modifyCityUppercase =
  error "todo: modifyCityUppercase"
