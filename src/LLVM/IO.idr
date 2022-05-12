module LLVM.IO

import Control.Linear.LIO

import LLVM.Primitives
import LLVM.Types

public export
writeBitcodeToFile : LinearIO io => (1 _ : Module) -> String -> L io ()
writeBitcodeToFile (MkModule m) filename =
  primIO $ prim__writeBitcodeToFile m filename
