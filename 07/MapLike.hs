import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

newtype ListMap k v = ListMap [(k,v)]

newtype ArrMap k v = ArrMap (k -> Maybe v)

-- 3. Написать instace Functor для ListMap k и ArrMap k.
