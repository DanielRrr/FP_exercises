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

uniprod :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
uniprod f g (x,y) = (f x, g y)

product :: Lens a b -> Lens c d -> Lens (a, c) (b, d)
product l1 l2 = Lens
                  (\x -> Store
                    (\p ->
                      ((set l1) (fst x) (fst p), (set l2) (snd x) (snd p)) )
                        (uniprod (get l1) (get l2) x))

(***) :: Lens a b -> Lens c d -> Lens (a, c) (b, d)
(***) = product

infixr 3 ***

choice :: Lens a c -> Lens b c -> Lens (Either a b) c
choice l1 l2 = Lens (\d -> Store (\e -> case d of
                    Left x -> Left (set l1 x e)
                    Right y -> Right (set l2 y e)
                    ) (either (get l1) (get l2) d))

(|||) :: Lens a x -> Lens b x -> Lens (Either a b) x
(|||) = choice

infixr 2 |||

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

ageL :: Lens Person Int
ageL = Lens (\(Person a n d) -> Store (\a' -> Person a' n d) a)

nameL :: Lens Person String
nameL = Lens (\(Person a n d) -> Store (\n' -> Person a n' d) n)

addressL :: Lens Person Address
addressL = Lens (\(Person a n d) -> Store (\d' -> Person a n d') d)

getSuburb :: Person -> String
getSuburb = get $ suburbL |. addressL

setStreet :: Person -> String -> Person
setStreet = set $ streetL |. addressL

getAgeAndCountry :: (Person, Locality) -> (Int, String)
getAgeAndCountry = get $ ageL *** countryL

setCityAndLocality :: (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality = set $ (cityL |. localityL |. addressL) *** localityL

getSuburbOrCity :: Either Address Locality -> String
getSuburbOrCity = get $ suburbL ||| cityL

setStreetOrState :: Either Person Locality -> String -> Either Person Locality
setStreetOrState = undefined

modifyCityUppercase :: Person -> Person
modifyCityUppercase = undefined
