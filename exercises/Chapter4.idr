module Chapter4

data BSTree : Type -> Type where
    Empty : Ord a => BSTree a
    Node : Ord a => (left : BSTree a) -> (val : a) -> (right : BSTree a) -> BSTree a

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
    = case compare x val of
        LT => Node (insert x left) val right
        EQ => orig
        GT => Node left val (insert x right)

-- Exercise 1
listToTree : Ord a => List a -> BSTree a
listToTree = foldl (flip insert) Empty

-- Exercise 2
treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node l v r) = treeToList l ++ [ v ] ++ treeToList r

-- Exercise 3
data Expr : Type where
    Const : Integer -> Expr
    Add : Expr -> Expr -> Expr
    Sub : Expr -> Expr -> Expr
    Mul : Expr -> Expr -> Expr

-- Exercise 4
evaluate : Expr -> Integer 
evaluate (Const i) = i
evaluate (Add l r) = evaluate l + evaluate r
evaluate (Sub l r) = evaluate l - evaluate r
evaluate (Mul l r) = evaluate l * evaluate r

-- Exercise 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe x Nothing = x
maxMaybe Nothing x = x
maxMaybe x y = max x y

x : Int
x = 5

f : Bool -> Bool
f = \x => x