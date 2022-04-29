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
        ]
