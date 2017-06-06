# Variants

Welcome to my grand scalameta experiment.

The purpose is to produce several different versions of your favourite algebra, given only a few annotations. 

This is immediately useful whenever you have variations of the same algebra: 
- different versions of a schema, 
- similar but different sets of domain objects, 
- any kind of compiler-like pipeline which needs a changing set of fields throughout.
  
```scala
  @Variants("One", "Two")
  trait Base {
    trait Animal[+T]
    @Include("Two") trait LivingAnimal[+T] extends Animal[T]
    case class Rhino[T](@Exclude("One") weight: Int, friends: Seq[Animal[T]], @Include("Two") secrets: Option[Seq[T]]) extends LivingAnimal[T] @Include("Two") with Animal[T] @Include("One")
    case class Dino[T](height: Int, @Include("Two") enemy: Option[Animal[T]]) extends Animal[T]
    @Include("Two")
    case object Dodo extends Animal[Nothing]
  }
}
```

The `Variants` macro will expand this to the following two ADTs:

```scala

  object One {
    trait Animal[+T]
    case class Rhino[T](friends: Seq[Animal[T]]) extends Animal[T]()
    case class Dino[T](height: Int) extends Animal[T]()
  }
  object Two {
    trait Animal[+T]
    trait LivingAnimal[+T] extends Animal[T]()
    case class Rhino[T](weight: Int, friends: Seq[Animal[T]], secrets: Option[Seq[T]]) extends LivingAnimal[T]()
    case class Dino[T](height: Int, enemy: Option[Animal[T]]) extends Animal[T]()
    case object Dodo extends Animal[Nothing]()
  }
```
The `@Include` and `@Exclude` can be used for types, members and inheritance.

## Boilerplate  

Since the author has a lot of huge trees to navigate and little patience for boilerplate, 
there is also some added goodness to help with those problems:

### Transformer

```scala
@Variants("One", "Two")
@Transformer
trait Base {/* as before */}
```

This will expand to this:
```scala
  object One {
    trait Animal[+T]
    case class Rhino[T](friends: Seq[Animal[T]]) extends Animal[T]()
    case class Dino[T](height: Int) extends Animal[T]()
    class OneTransformer[Scope, T](implicit newscope: variants.NewScope[Scope, Animal[T]], SeqFunctor: variants.Functor[Seq]) {
      def visitAnimal(scope: Scope)(_0: Animal[T]): Animal[T] = _0 match {
        case x: Dino[T] =>
          visitDino(scope)(x)
        case x: Rhino[T] =>
          visitRhino(scope)(x)
      }
      def enterDino(scope: Scope)(_0: Dino[T]): Dino[T] = _0
      final def visitDino(scope: Scope)(_0: Dino[T]): Dino[T] = {
        val _1: Dino[T] = enterDino(scope)(_0)
        lazy val childScope: Scope = newscope.derive(scope, _1)
        val _2: Dino[T] = new Dino(height = _1.height)
        _2
      }
      def enterRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = _0
      final def visitRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = {
        val _1: Rhino[T] = enterRhino(scope)(_0)
        lazy val childScope: Scope = newscope.derive(scope, _1)
        val _2: Rhino[T] = new Rhino(friends = SeqFunctor.map(_1.friends)(x => visitAnimal(childScope)(x)))
        _2
      }
    }
  }
  object Two {
    trait Animal[+T]
    trait LivingAnimal[+T] extends Animal[T]()
    case class Rhino[T](weight: Int, friends: Seq[Animal[T]], secrets: Option[Seq[T]]) extends LivingAnimal[T]()
    case class Dino[T](height: Int, enemy: Option[Animal[T]]) extends Animal[T]()
    case object Dodo extends Animal[Nothing]()
    class TwoTransformer[Scope, T](implicit newscope: variants.NewScope[Scope, Animal[T]], OptionFunctor: variants.Functor[Option], SeqFunctor: variants.Functor[Seq]) {
      def visitAnimal(scope: Scope)(_0: Animal[T]): Animal[T] = _0 match {
        case x: Dino[T] =>
          visitDino(scope)(x)
        case x: Dodo.type =>
          visitDodo(scope)(x)
        case x: LivingAnimal[T] =>
          visitLivingAnimal(scope)(x)
      }
      def visitLivingAnimal(scope: Scope)(_0: LivingAnimal[T]): LivingAnimal[T] = _0 match {
        case x: Rhino[T] =>
          visitRhino(scope)(x)
      }
      def enterDino(scope: Scope)(_0: Dino[T]): Dino[T] = _0
      final def visitDino(scope: Scope)(_0: Dino[T]): Dino[T] = {
        val _1: Dino[T] = enterDino(scope)(_0)
        lazy val childScope: Scope = newscope.derive(scope, _1)
        val _2: Dino[T] = new Dino(height = _1.height, enemy = OptionFunctor.map(_1.enemy)(x => visitAnimal(childScope)(x)))
        _2
      }
      final def visitDodo(scope: Scope)(_0: Dodo.type): Dodo.type = enterDodo(scope)(_0)
      def enterDodo(scope: Scope)(_0: Dodo.type): Dodo.type = _0
      def enterRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = _0
      final def visitRhino(scope: Scope)(_0: Rhino[T]): Rhino[T] = {
        val _1: Rhino[T] = enterRhino(scope)(_0)
        lazy val childScope: Scope = newscope.derive(scope, _1)
        val _2: Rhino[T] = new Rhino(weight = _1.weight, friends = SeqFunctor.map(_1.friends)(x => visitAnimal(childScope)(x)), secrets = _1.secrets)
        _2
      }
    }
  }

```
By overriding the `visitX` methods, the user is in full control over transformations of the tree. 
The `NewScope` typeclass builds up a current scope which contains the full path to the current object. 

The navigation is all typeclass-driven. When the macro comes across a type constructor 
(only one type param supported) which is not defined within the ADT, it requires a `Functor` to transform it. 
It will only do this if there are members of the ADTs contained within.


### Functor
For ADTs with a type parameter (like `Animal`), you can also generate a `Functor` for it.

```scala
@Variants("One", "Two")
@FunctorAnn
trait Base {/* as before */}
```

```scala
  object One {
    trait Animal[+T]
    case class Rhino[T](friends: Seq[Animal[T]]) extends Animal[T]()
    case class Dino[T](height: Int) extends Animal[T]()
    class OneFunctors(implicit SeqFunctor: Functor[Seq]) {
      implicit lazy val AnimalFunctor: Functor[Animal] = new Functor[Animal] {
        def map[T, TT](x: Animal[T])(f: T => TT): Animal[TT] = x match {
          case x: Dino[T] =>
            DinoFunctor.map(x)(f)
          case x: Rhino[T] =>
            RhinoFunctor.map(x)(f)
        }
      }
      implicit lazy val DinoFunctor: Functor[Dino] = new Functor[Dino] { def map[T, TT](x: Dino[T])(f: T => TT): Dino[TT] = new Dino(height = x.height) }
      implicit lazy val RhinoFunctor: Functor[Rhino] = new Functor[Rhino] { def map[T, TT](x: Rhino[T])(f: T => TT): Rhino[TT] = new Rhino(friends = SeqFunctor.map(x.friends)(x => AnimalFunctor.map(x)(x => f(x)))) }
    }
  }
  object Two {
    trait Animal[+T]
    trait LivingAnimal[+T] extends Animal[T]()
    case class Rhino[T](weight: Int, friends: Seq[Animal[T]], secrets: Option[Seq[T]]) extends LivingAnimal[T]()
    case class Dino[T](height: Int, enemy: Option[Animal[T]]) extends Animal[T]()
    case object Dodo extends Animal[Nothing]()
    class TwoFunctors(implicit OptionFunctor: Functor[Option], SeqFunctor: Functor[Seq]) {
      implicit lazy val AnimalFunctor: Functor[Animal] = new Functor[Animal] {
        def map[T, TT](x: Animal[T])(f: T => TT): Animal[TT] = x match {
          case x: Dino[T] =>
            DinoFunctor.map(x)(f)
          case x: Dodo.type =>
            x
          case x: LivingAnimal[T] =>
            LivingAnimalFunctor.map(x)(f)
        }
      }
      implicit lazy val DinoFunctor: Functor[Dino] = new Functor[Dino] { def map[T, TT](x: Dino[T])(f: T => TT): Dino[TT] = new Dino(height = x.height, enemy = OptionFunctor.map(x.enemy)(x => AnimalFunctor.map(x)(x => f(x)))) }
      implicit lazy val LivingAnimalFunctor: Functor[LivingAnimal] = new Functor[LivingAnimal] {
        def map[T, TT](x: LivingAnimal[T])(f: T => TT): LivingAnimal[TT] = x match {
          case x: Rhino[T] =>
            RhinoFunctor.map(x)(f)
        }
      }
      implicit lazy val RhinoFunctor: Functor[Rhino] = new Functor[Rhino] { def map[T, TT](x: Rhino[T])(f: T => TT): Rhino[TT] = new Rhino(weight = x.weight, friends = SeqFunctor.map(x.friends)(x => AnimalFunctor.map(x)(x => f(x))), secrets = OptionFunctor.map(x.secrets)(x => SeqFunctor.map(x)(x => f(x)))) }
    }
  }
```

This is again completely typeclass-driven, so in the same manner as for`Visitor`, you need to instantiate a `class` to partially apply the 
external `Functor`s.

## Limitations
Several. You cannot be very fancy with type parameters or aliases. This macro is written without the semantic API, 
so any type comparison is a `String` comparison, really. Considering how easy it was to write, I'm not sure that
is even a huge downside, though :)
  