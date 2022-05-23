module LLVM.IO

import System.FFI
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
                             L1 io (ModuleWith AnyPtr)
writeBitcodeToMemoryBuffer (MkModule m) = do
  buf <- primIO $ prim__writeBitcodeToMemoryBuffer m
  pure1 $ M (MkModule m) buf

public export
writeBitcodeToMemoryBuffer' : LinearIO io => (1 mod : Module) -> L io AnyPtr
writeBitcodeToMemoryBuffer' mod = do
  M mod buf <- writeBitcodeToMemoryBuffer mod
  disposeModule mod
  pure buf

public export
printModuleToString : LinearIO io => (1 mod : Module) ->
                      L1 io (ModuleWith String)
printModuleToString (MkModule m) = do
  str <- primIO $ prim__printModuleToString m
  pure1 $ M (MkModule m) str

public export
printModuleToString' : LinearIO io => (1 mod : Module) -> L io String
printModuleToString' mod = do
  M mod str <- printModuleToString mod
  disposeModule mod
  pure str
