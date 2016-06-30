-- Assignment 3 - 2/7/16 - Aidan Gadberry agadberr

data BST k v = Empty |
               Node k v (BST k v) (BST k v)

data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]
-- BST and JSON data

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ v _ _) = Just v

size :: BST k v -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r

ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node k2 v2 l r) = if (k < k2)
                           then Node k2 v2 (ins k v l) r
                           else if (k > k2)
                           then Node k2 v2 l (ins k v r)
                           else Node k v r l

instance (Show v) => Show (BST k v) where
    show Empty = ""
    show (Node k v left right) = "(" ++ (show left) ++ (show v) ++ (show right) ++ ")"


instance Show JSON where
    show = showJson
-- function showJson is called whenever we try to "show" a JSON

showJson :: JSON -> String
showJson (JStr s) = "\"" ++ s ++ "\""
showJson (JNum d) = show d
showJson (JArr js) = init ("[" ++ (foldr (\x acc -> (showJson x) ++ "," ++ acc) [] js)) ++ "]"
showJson (JObj kvs) = init ("{" ++ (foldr (\(x,y) acc -> show x ++ ":" ++ (showJson y) ++ "," ++ acc) [] kvs)) ++ "}"


class Json a where
    toJson :: a -> JSON
    fromJson :: JSON -> a
-- Json class where all Json things include toJson and fromJson functions

instance Json Double where
    toJson = JNum
    fromJson = read . showJson

instance (Json a) => Json [a] where
    toJson = JArr . map toJson
    fromJson (JArr xs) = map fromJson xs
