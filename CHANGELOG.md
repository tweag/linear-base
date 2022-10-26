# Change Log

## [v0.3.0](https://github.com/tweag/linear-base/tree/v0.3.0) (2022-10-26)

[Full Changelog](https://github.com/tweag/linear-base/compare/v0.2.0...v0.3.0)

### Headline changes

- Support GHC 9.4 in addition to GHC 9.2 and GHC 9.0 (GHC 9.0 is no longer tested though) [\#427](https://github.com/tweag/linear-base/pull/427) ([matthew-healy](https://github.com/matthew-healy))
  - Update `shell.nix` to enable building with GHC 9.4.1 [\#429](https://github.com/tweag/linear-base/pull/429) ([matthew-healy](https://github.com/matthew-healy))
- Improvement of the resource-aware `IO` (`RIO`) monad's interface
  - Add some `Handle` operations to `RIO` [\#425](https://github.com/tweag/linear-base/pull/425) ([endgame](https://github.com/endgame))
  - The `Handle` type is now transparent, to make extensions of the API possible [\#428](https://github.com/tweag/linear-base/pull/428) ([aspiwack](https://github.com/aspiwack))

### Miscellaneous

- `Monoid (Maybe a)` requires only `Semigroup a`, not `Monoid a` [\#409](https://github.com/tweag/linear-base/pull/409) ([treeowl](https://github.com/treeowl))
- Add `evalState(T)` [\#411](https://github.com/tweag/linear-base/pull/411) ([andreasabel](https://github.com/andreasabel))
- Add `Movable` instances for several primitive types, as well as (non-linear) `Applicative`, `Foldable`, `Traversable` instances for `V`. [\#416](https://github.com/tweag/linear-base/pull/416) ([sellout](https://github.com/sellout))
- Fix typo in comment: resrouce -\> resource [\#421](https://github.com/tweag/linear-base/pull/421) ([undergroundquizscene](https://github.com/undergroundquizscene))
- Fix haddock hyperlink reference \(minor typo\) [\#420](https://github.com/tweag/linear-base/pull/420) ([undergroundquizscene](https://github.com/undergroundquizscene))
- Fix haddock links in `Data.Replicator.Linear` [\#423](https://github.com/tweag/linear-base/pull/423) ([undergroundquizscene](https://github.com/undergroundquizscene))
- Add `CONTRIBUTING.md` [\#426](https://github.com/tweag/linear-base/pull/426) ([tbagrel1](https://github.com/tbagrel1))

### Internal

- Upgrade GHC to 9.2 [\#414](https://github.com/tweag/linear-base/pull/414) ([aspiwack](https://github.com/aspiwack))
  - Don't use deprecated `testProperty` from tasty-hedgehog [\#415](https://github.com/tweag/linear-base/pull/415) ([aspiwack](https://github.com/aspiwack))

## [v0.2.0](https://github.com/tweag/linear-base/tree/v0.2.0) - 2022-03-25

[Full Changelog](https://github.com/tweag/linear-base/compare/v0.1.0...v0.2.0)

### Breaking changes

- Remove `Prelude.Linear.asTypeOf` [\#397](https://github.com/tweag/linear-base/pull/397) ([tbagrel1](https://github.com/tbagrel1))
- Add (and use) linear generics for many classes [\#394](https://github.com/tweag/linear-base/pull/394) ([treeowl](https://github.com/treeowl))
  - `Control.Functor.Linear.Functor`, `Data.Functor.Linear.{Functor,Applicative}` can now be derived through `Generically1`
  - `Data.Functor.Linear.Traversable` cannot be derived directly, but one can get `genericTraverse` for a `Generic1` type and then set `traverse = genericTraverse`
  - `Data.Unrestricted.Linear.{Consumable,Dupable,Movable}` can be derived through `Generically`
- Rework `Data.Monoid.Linear` module (affects linear `Semigroup` and `Monoid`) [\#314](https://github.com/tweag/linear-base/pull/314) ([sjoerdvisscher](https://github.com/sjoerdvisscher)), [\#381](https://github.com/tweag/linear-base/pull/381) ([tbagrel1](https://github.com/tbagrel1))
  - **Remove superclass constraint** on `Prelude.{Semigroup,Monoid}` for `Data.Monoid.Linear.{Semigroup,Monoid}`. `Data.Monoid.Linear.Monoid` instances now have to define `mempty`
  - Add many missing instances of `Data.Monoid.Linear.{Semigroup,Monoid}`
  - Deprecate `Data.Monoid.Linear.{Adding,Multiplying,getAdded,getMultiplied}` in favor of `Data.Semigroup.{Sum,Product}` (reexported under `Data.Monoid.Linear`) which now have linear `Semigroup` and `Monoid` instance. `Sum` and `Product` inner values can be extracted linearly with pattern-matching
  - **`Data.Semigroup` is no longer reexported as a whole under `Data.Monoid.Linear`**. Instead, only newtypes with a linear `Semigroup` instance are reexported
- Add missing fixity declarations for every operator of `linear-base` [\#386](https://github.com/tweag/linear-base/pull/386), ([tbagrel1](https://github.com/tbagrel1))
  - **Unchanged** (already present):<br/>`infixr 0 $`, `infixl 1 &`, `infixr 2 ||`, `infixr 3 &&`,<br/> `infix 4 ==, /=, <=, <, >, >=`, `infixr 5 :>`
  - Add: ```infixr 0 `lseq`, `seq`, $!```
  - Add: `infixl 1 <&>, >>=, >>, &`
  - Add: `infixr 3 ***`
  - Add: ```infix 4 `compare`, `elem` ```
  - Add: `infixl 4 <$>, <$, <*>, <*`
  - Add: `infixr 5 ++`
  - Add: `infixr 6 <>`
  - Add: `infixl 6 +, -`
  - Add: `infixl 7 *`
  - Add: `infixr 9 #., .>, .`
  - **Previously missing fixity declarations defaulted to `infixl 9`, so some code might subtly break when updating to v0.2.0**
- Improve consistency of module naming [\#383](https://github.com/tweag/linear-base/pull/383) ([tbagrel1](https://github.com/tbagrel1))
  - **`System.IO.Resource` -> `System.IO.Resource.Linear`**
- Rework `Data.V.Linear` API [\#360](https://github.com/tweag/linear-base/pull/360) ([tbagrel1](https://github.com/tbagrel1))
  - `Data.Functor.Linear.Applicative` instance
  - `empty :: forall a. V 0 a`
  - `consume :: V 0 a %1 -> ()`
  - `cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a`
  - `uncons# :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)`
  - `uncons :: 1 <= n => V n a %1 -> (a, V (n - 1) a)`
  - `elim :: forall (n :: Nat) a b f. IsFunN a b f => f %1 -> V n a %1 -> b`
  - `make :: forall (n :: Nat) a f. IsFunN a (V n a) f => f`
  - `fromReplicator :: forall n a. KnownNat n => Replicator a %1 -> V n a`
  - `theLength :: forall n. KnownNat n => Prelude.Int`
  - **`dupV` is now part of `Data.V.Linear`:**<br/>
    `dupV :: forall n a. (KnownNat n, Dupable a) => a %1 -> V n a`
- Replace `dupV` in the minimal definition of `Data.Unrestricted.Linear.Dupable` with `dupR :: a %1 -> Replicator a` [\#360](https://github.com/tweag/linear-base/pull/360) ([tbagrel1](https://github.com/tbagrel1)) [\#365](https://github.com/tweag/linear-base/pull/365) ([facundominguez](https://github.com/facundominguez))
  - Introduce a new data type `Data.Replicator.Linear.Replicator`, which represents an infinite linear stream producing values of type `a`, with a stream-like API and a `Data.Functor.Linear.Applicative` instance
  - `Data.Unrestricted.Linear.Dupable` no longer depends on `Data.V.Linear`
  - Add `dup3`, `dup4`, `dup5`, `dup6`, `dup7`
- Polymorphise the type of some `Prelude.Linear` functions in levity and multiplicity [\#353](https://github.com/tweag/linear-base/pull/353) ([treeowl](https://github.com/treeowl))
  - `($) :: forall {rep} a (b :: TYPE rep) p q. (a %p-> b) %q-> a %p-> b`
  - `(&) :: forall {rep} a (b :: TYPE rep) p q. a %p-> (a %p-> b) %q-> b`
  - `($!) :: forall {rep} a (b :: TYPE rep) p q. (a %p-> b) %q-> a %p-> b`
  - `(.) :: forall {rep} b (c :: TYPE rep) a q m n. (b %1-> c) %q-> (a %1-> b) %m-> a %n-> c`
  - `forget :: forall {rep} a (b :: TYPE rep). (a %1-> b) %1-> a -> b`
- Multiplicity-polymorphise the type of some `Prelude.Linear` functions [\#319](https://github.com/tweag/linear-base/pull/319) ([aspiwack](https://github.com/aspiwack))
  - `id :: a %q-> a`
  - `const :: a %q-> b -> a`
  - `asTypeOf :: a %q-> a -> a`
  - `seq :: a -> b %q-> b`
  - `curry :: ((a, b) %p-> c) %q-> a %p-> b %p-> c`
  - `uncurry :: (a %p-> b %p-> c) %q-> (a, b) %p-> c`
  - `runIdentity' :: Identity a %p-> a`
- Remove `LinearArrow` usage in `Control.Optics.Linear` and use `FUN 'One` instead [\#308](https://github.com/tweag/linear-base/pull/308) ([sjoerdvisscher](https://github.com/sjoerdvisscher))
  - This change add a `Data.Profunctor.Linear.Profunctor` instance to `FUN 'One`
- Add `.Linear.Internal` modules (and only export parts of them in publicly-exposed `.Linear` modules) [\#306](https://github.com/tweag/linear-base/pull/306) ([ekmett](https://github.com/ekmett))
  - `Data.Array.Destination[.Internal]`
  - `Data.Array.Mutable.Linear[.Internal]`
  - `Data.HashMap.Mutable.Linear[.Internal]`
  - `Data.Set.Mutable.Linear[.Internal]`
  - `Data.Vector.Mutable.Linear[.Internal]`
  - `Foreign.Marshal.Pure[.Internal]`
  - `System.IO.Resource.Linear.[.Internal]`
  - This principle has been applied for newly-created modules in the subsequent PRs

### New additions

- Add `Data.Arity.Linear` module containing type-level helpers to deal with n-ary linear functions and type-level structural integers [\#390](https://github.com/tweag/linear-base/pull/390) ([aspiwack](https://github.com/aspiwack)), [\#391](https://github.com/tweag/linear-base/pull/391) ([tbagrel1](https://github.com/tbagrel1))
- Add `void` function to consume `Control.Functor.Linear.Functor` inner value [\#387](https://github.com/tweag/linear-base/pull/387) ([tbagrel1](https://github.com/tbagrel1))
- Add inspection tests to check inlining of `Data.Replicator.Linear.elim` and `Data.V.Linear.{make,elim}` [\#367](https://github.com/tweag/linear-base/pull/367) ([tbagrel1](https://github.com/tbagrel1))
- Add `genericTraverse` to `Data.Functor.Linear` for `Generics.Linear.Generic1` types [\#366](https://github.com/tweag/linear-base/pull/366) ([tbagrel1](https://github.com/tbagrel1)), [\#384](https://github.com/tweag/linear-base/pull/384) ([aspiwack](https://github.com/aspiwack)), [\#385](https://github.com/tweag/linear-base/pull/385) ([treeowl](https://github.com/treeowl)) 
- Add `Unsafe.toLinearN` (and narrow the scope of some coercions in the module internals) [\#346](https://github.com/tweag/linear-base/pull/346) ([treeowl](https://github.com/treeowl))
- Add newtype `Data.Unrestricted.Linear.AsMovable` to derive `Consumable` and `Dupable` from `Movable` [\#357](https://github.com/tweag/linear-base/pull/357) ([tbagrel1](https://github.com/tbagrel1))
- Add `Data.Unrestricted.Linear.{Consumable,Dupable,Moveable}` instances for all Word and Int types [\#352](https://github.com/tweag/linear-base/pull/352) ([googleson78](https://github.com/googleson78))
- Add benchmarks for `Data.HashMap.Linear` [\#338](https://github.com/tweag/linear-base/pull/338) ([utdemir](https://github.com/utdemir))
- Add benchmarks for `Data.Array.Mutable.Linear` [\#331](https://github.com/tweag/linear-base/pull/331) ([utdemir](https://github.com/utdemir))
- Add `Data.Unrestricted.Linear.{Consumable,Dupable}` instances to `Data.V.Linear.V` [\#324](https://github.com/tweag/linear-base/pull/324) ([aspiwack](https://github.com/aspiwack))
- Add `Data.Unrestricted.Linear.UrT`, the unrestricted monad transformer [\#304](https://github.com/tweag/linear-base/pull/304) ([sjoerdvisscher](https://github.com/sjoerdvisscher))

### Code improvements

- Add robustness improvements to `Data.Replicator.Linear.elim` and `Data.V.Linear.{make,elim}` [\#364](https://github.com/tweag/linear-base/pull/364) ([tbagrel1](https://github.com/tbagrel1)), [\#382](https://github.com/tweag/linear-base/pull/382) ([tbagrel1](https://github.com/tbagrel1)), [\#390](https://github.com/tweag/linear-base/pull/390) ([aspiwack](https://github.com/aspiwack)), [\#391](https://github.com/tweag/linear-base/pull/391) ([tbagrel1](https://github.com/tbagrel1))
- Add various optimisations for `Data.HashMap.Linear` [\#337](https://github.com/tweag/linear-base/pull/337) ([utdemir](https://github.com/utdemir))
- Improve `Data.Array.Mutable.Unlifted.Linear.map` performance [\#334](https://github.com/tweag/linear-base/pull/334) ([utdemir](https://github.com/utdemir))
- Remove one `unsafeCoerce` use from `Unsafe.coerce` [\#330](https://github.com/tweag/linear-base/pull/330) ([utdemir](https://github.com/utdemir))
- Improve `Prelude.Linear.seq` performance [\#329](https://github.com/tweag/linear-base/pull/329) ([utdemir](https://github.com/utdemir))
- Use safer `Vector.fromArray` in `Data.Array.Mutable.Linear` internals [\#327](https://github.com/tweag/linear-base/pull/327) ([utdemir](https://github.com/utdemir))
- Remove some incomplete pattern matches in `Data.List.Linear.{scanr,scanr1}` [\#299](https://github.com/tweag/linear-base/pull/299) ([utdemir](https://github.com/utdemir))

### CI/Tooling improvements

- Move CI tests from the `cabal` job to the `stack` job [\#398](https://github.com/tweag/linear-base/pull/398) ([tbagrel1](https://github.com/tbagrel1))
- Set warnings for `ghcide` in the cabal file [\#378](https://github.com/tweag/linear-base/pull/378) ([aspiwack](https://github.com/aspiwack))
- Disable all `hlint` hints except those related to pragmas [\#362](https://github.com/tweag/linear-base/pull/362) ([tbagrel1](https://github.com/tbagrel1))
- Enable doctesting through `cabal-docspec` in the CI [\#361](https://github.com/tweag/linear-base/pull/361) ([andreabedini](https://github.com/andreabedini))
- Format the codebase with [`ormolu`](https://github.com/tweag/ormolu) and add an `ormolu` check to the CI [\#355](https://github.com/tweag/linear-base/pull/355) ([tbagrel1](https://github.com/tbagrel1)), [\#358](https://github.com/tweag/linear-base/pull/358) ([tbagrel1](https://github.com/tbagrel1))
- CI and `shell.nix` overhaul [\#322](https://github.com/tweag/linear-base/pull/322) ([aspiwack](https://github.com/aspiwack)), [\#323](https://github.com/tweag/linear-base/pull/323) ([aspiwack](https://github.com/aspiwack)), [\#325](https://github.com/tweag/linear-base/pull/325) ([utdemir](https://github.com/utdemir)), [\#332](https://github.com/tweag/linear-base/pull/332) ([utdemir](https://github.com/utdemir)), [\#348](https://github.com/tweag/linear-base/pull/348) ([aspiwack](https://github.com/aspiwack)), [\#355](https://github.com/tweag/linear-base/pull/355) ([tbagrel1](https://github.com/tbagrel1)), [\#359](https://github.com/tweag/linear-base/pull/359) ([tbagrel1](https://github.com/tbagrel1))
  - Bump `nixpkgs` and `stackage` pinned versions to recent `unstable`/`nightly` ones
  - Move CI from *Buildkite* to *Github Action*
  - Automatically run the CI on pull requests
  - Add `stack`/Nix integration when `stack` is provided by `nix-shell`/`shell.nix` (the project still builds with a globally installed `stack`)
- Force resolving test dependencies on `cabal` [\#342](https://github.com/tweag/linear-base/pull/342) ([utdemir](https://github.com/utdemir))
- Remove `cabal-docspec` reference from `Setup.hs` [\#335](https://github.com/tweag/linear-base/pull/335) ([facundominguez](https://github.com/facundominguez))
- Start using upstream `nixpkgs` (instead of our own fork) [\#302](https://github.com/tweag/linear-base/pull/302) ([utdemir](https://github.com/utdemir))

### Documentation improvements

- Change relative links for absolute ones in the README [\#401](https://github.com/tweag/linear-base/pull/401) ([tbagrel1](https://github.com/tbagrel1))
- Add comparison table between `Prelude` and `Prelude.Linear` classes [\#368](https://github.com/tweag/linear-base/pull/368) ([tbagrel1](https://github.com/tbagrel1))
- Add Hackage and Stackage badges [\#336](https://github.com/tweag/linear-base/pull/336) ([utdemir](https://github.com/utdemir))
- Hide internal modules from `haddock` documentation [\#326](https://github.com/tweag/linear-base/pull/326) ([utdemir](https://github.com/utdemir)), [\#363](https://github.com/tweag/linear-base/pull/363) ([tbagrel1](https://github.com/tbagrel1))
- Add a note that GHC 9.2 fixes linear `case` in the user guide [\#320](https://github.com/tweag/linear-base/pull/320) ([monoidal](https://github.com/monoidal))
- Replace `#->` with `%1 ->` in the documentation [\#315](https://github.com/tweag/linear-base/pull/315) ([sjoerdvisscher](https://github.com/sjoerdvisscher))
- Fix rendering in `Data.Unrestricted.Linear.Ur` documentation [\#303](https://github.com/tweag/linear-base/pull/303) ([sjoerdvisscher](https://github.com/sjoerdvisscher))
- Fix a typo in `Data.Array.Mutable.Linear.unsafeWrite` documentation [\#301](https://github.com/tweag/linear-base/pull/301) ([daig](https://github.com/daig))
- Add a list of introduction talks about linear types in the README [\#300](https://github.com/tweag/linear-base/pull/300) ([aspiwack](https://github.com/aspiwack))
- Improve developer documentation in `Data.Array.Polarized.Push` [\#294](https://github.com/tweag/linear-base/pull/294) ([Divesh-Otwani](https://github.com/Divesh-Otwani))


## [v0.1.0](https://github.com/tweag/linear-base/tree/v0.1.0) - 2021-02-09

* Initial release
