module LLVM.Module

import Control.Linear.LIO

import LLVM.Primitives
import LLVM.Types

||| Creates a module with the given name.
public export
createModuleWithName : LinearIO io =>
                       (name : String) ->
                       (1 ctxt : Context) ->
                       L1 io (LPair Context Module)
createModuleWithName name (MkCtxt c) = do
  m <- primIO $ prim__createModuleWithNameInContext name c
  pure1 (MkCtxt c # MkModule m)

public export
disposeModule : HasIO io => (1 mod : Module) -> io ()
disposeModule (MkModule m) = primIO $ prim__disposeModule m
