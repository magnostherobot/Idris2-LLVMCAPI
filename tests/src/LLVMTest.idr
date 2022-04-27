module LLVMTest

import Test.Unit
import Data.Vect
import Control.App

import LLVM

public export
tests : PrimIO es => List (Test es)
tests = [ MkTest "createModuleWithName does not crash" $ do
          _ <- createModuleWithName "test_module"
          pass

        , MkTest "createModuleWithName does not crash on empty name" $ do
          _ <- createModuleWithName ""
          pass

        , MkTest "addFunction does not crash" $ do
          m <- createModuleWithName "test_module"
          t <- functionType voidType [] False
          _ <- addFunction m "test_func" t
          pass

        , MkTest "addFunction does not crash on empty name" $ do
          m <- createModuleWithName "test_module"
          t <- functionType voidType [] False
          _ <- addFunction m "" t
          pass
        ]
