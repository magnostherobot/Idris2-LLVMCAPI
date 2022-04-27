module LLVM.IO

import LLVM.Primitives
import LLVM.Types

public export
writeBitcodeToFile : HasIO io => Module -> String -> io ()
writeBitcodeToFile (MkModule m) filename =
  primIO $ prim__writeBitcodeToFile m filename
