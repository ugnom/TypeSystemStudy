

identity :: a -> a
identity x = x

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

apply :: (a -> b) -> a -> b
apply f x = f x	