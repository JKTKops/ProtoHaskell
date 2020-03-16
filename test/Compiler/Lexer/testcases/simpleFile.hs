module SimpleFile where

foo :: Foo
foo = undefined

bar :: (Bar -> Foo) -> Bar -> Foo
bar f b = f b

bar' :: (Bar -> Foo) -> Bar -> Foo
bar' = id
