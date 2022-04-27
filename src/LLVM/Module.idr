module LLVM.Module

import LLVM.Primitives
import LLVM.Types

||| Creates a module with the given name.
public export
createModuleWithName : HasIO io => String -> io Module
createModuleWithName name =
  pure $ MkModule !(primIO $ prim__createModuleWithName name)
