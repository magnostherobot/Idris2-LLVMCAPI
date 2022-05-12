module LLVM.Module

import Control.Linear.LIO

import LLVM.Primitives
import LLVM.Types

||| Creates a module with the given name.
public export
createModuleWithName : LinearIO io => String -> L1 io Module
createModuleWithName name =
  pure1 $ MkModule !(primIO $ prim__createModuleWithName name)
