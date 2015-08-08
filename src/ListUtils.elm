module ListUtils (find, forceTail) where

find : (a -> Bool) -> List a -> Maybe a
find p l = List.foldl 
  (\a b -> 
    case b of
      Nothing -> if p a then Just a else Nothing
      Just a  -> Just a)
  Nothing
  l

forceTail : List a -> List a
forceTail l = case List.tail l of
  Nothing -> []
  Just a -> a