module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = doctest args'
  where
    args = flags ++ pkgs ++ module_sources
    args' = [ "-i",
              "-i/home/utdemir/workspace/github.com/tweag/linear-base/dist-newstyle/build/x86_64-linux/ghc-9.0.1/linear-base-0.1.1/build/autogen",
              "-i/home/utdemir/workspace/github.com/tweag/linear-base/dist-newstyle/build/x86_64-linux/ghc-9.0.1/linear-base-0.1.1/build",
              -- "-i/home/utdemir/workspace/github.com/tweag/linear-base/src",
              "-package-env=-",
              "-hide-all-packages",
              "-no-user-package-db",
              "-package-db=/home/utdemir/.cabal/store/ghc-9.0.1/package.db",
              "-package-db=/home/utdemir/workspace/github.com/tweag/linear-base/dist-newstyle/packagedb/ghc-9.0.1",
              "-package-db=/home/utdemir/workspace/github.com/tweag/linear-base/dist-newstyle/build/x86_64-linux/ghc-9.0.1/linear-base-0.1.1/package.conf.inplace",
              "-optP-include",
              "-optP/home/utdemir/workspace/github.com/tweag/linear-base/dist-newstyle/build/x86_64-linux/ghc-9.0.1/linear-base-0.1.1/build/autogen/cabal_macros.h",
              "-package-id=base-4.15.0.0",
              "-package-id=containers-0.6.4.1",
              "-package-id=ghc-prim-0.7.0",
              "-package-id=hashable-1.3.1.0-a92c75af1fea2a65b2365eb935f4a4802dac5057158f909650e1011dc3a01a23",
              "-package-id=primitive-0.7.1.0-f003beddf0fc9d04aaf34af7ddb2d21771469f6e4225ce5a793266b89bfbb923",
              "-package-id=storable-tuple-0.0.3.3-7f76b1fd43fb107740fd5c1db4bf4e5a25de9b66b13ca4f53845700662363649",
              "-package-id=text-1.2.4.1",
              "-package-id=transformers-0.5.6.2",
              "-package-id=vector-0.12.3.0-8cc976946fcdbc43a65d82e2ca0ef40a7bb90b17e6cc65c288a8b694f5ac3127",
              "-package-id=doctest-0.18.1-ba4233742e9e8602bcf50dd58a39029934715a541fa504ea2e97f9de2502fb2a",
              "-package=linear-base",
              "src/Foreign/Marshal/Pure.hs"
            ]
