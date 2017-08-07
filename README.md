# Bismuth-Prototype

Bismuth is a purely functional programming language with first-class effects and an imperative feel.

This is a prototype implementation that is no longer under development. The new compiler will be published soon, with some major changes to the language.

## Hello, World!

```
module Main

func main! IO {
  putString "Hello, world."!;
}
```

## Functions

```
func add (x: Int) (y: Int) -> Int {
  return x + y;
}
```

## Generics

```
union List[a] {
  Nil,
  Cons (a, List a),
}

func map [a, b] (f: a -> b) (list: List a) -> List b {
  return match list {
    case Nil => Nil,
    case Cons (x: a, xs: List a) => Cons (f x) (map f xs),
  };
}
```

