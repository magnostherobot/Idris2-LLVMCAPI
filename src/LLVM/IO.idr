module LLVM.IO

import Control.Linear.LIO

import LLVM.Module
import LLVM.Primitives
import LLVM.Types
import LLVM.Value

public export
writeBitcodeToFile : LinearIO io => (1 mod : Module) -> String -> L1 io Module
writeBitcodeToFile (MkModule m) filename = do
  primIO $ prim__writeBitcodeToFile m filename
  pure1 (MkModule m)

public export
writeBitcodeToFile' : LinearIO io => (1 mod : Module) -> String -> L io ()
writeBitcodeToFile' mod filename = do
  mod <- writeBitcodeToFile mod filename
  disposeModule mod

public export
writeBitcodeToMemoryBuffer : LinearIO io => (1 mod : Module) ->
                             L1 io (ModuleWith String)
writeBitcodeToMemoryBuffer (MkModule m) = do
  buf <- primIO $ prim__writeBitcodeToMemoryBuffer m
  pure1 $ M (MkModule m) buf

public export
writeBitcodeToMemoryBuffer' : LinearIO io => (1 mod : Module) -> L io String
writeBitcodeToMemoryBuffer' mod = do
  M mod buf <- writeBitcodeToMemoryBuffer mod
  disposeModule mod
  pure buf
