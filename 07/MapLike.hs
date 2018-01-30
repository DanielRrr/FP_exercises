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
  empty :: MapLike m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap [(k,v)]

instance MapLike ListMap where
  empty = []

  lookup key (ListMap []) = Nothing
  lookup key (ListMap (x:xs)) = if key == (fst x) then (Just (snd x)) else lookup key (ListMap xs)


newtype ArrMap k v = ArrMap (k -> Maybe v)

-- 3. Написать instace Functor для ListMap k и ArrMap k.
