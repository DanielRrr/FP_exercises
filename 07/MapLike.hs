import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

instance MapLike M.Map where
  empty = M.empty
  lookup = M.lookup
  insert = M.insert
  delete = M.delete
  fromList = M.fromList

newtype ListMap k v = ListMap [(k,v)]

instance MapLike ListMap where
  empty = ListMap []
  lookup key (ListMap xs) = L.lookup key xs
  insert key value (ListMap xs) = ListMap $ filter (\p -> fst p /= key) xs ++ [(key,value)]
  delete key (ListMap xs) = ListMap $ filter (\p -> fst p /= key) xs

newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike ArrMap where
  empty = ArrMap (const Nothing)
  lookup key (ArrMap f) = f key
  insert key value (ArrMap f) =
    ArrMap $ (\x -> if key == x then (Just value) else f key)
  delete key (ArrMap f) = ArrMap $ (\x -> if key == x then Nothing else f key)

-- 3. Написать instace Functor для ListMap k и ArrMap k.

instance Functor (ListMap k) where
  -- (a -> b) -> ListMap k a -> ListMap k b
  fmap f (ListMap xs) = ListMap (map (fmap f) xs)
