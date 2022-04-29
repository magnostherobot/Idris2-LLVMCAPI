module LLVM.Context

import Data.Array

import LLVM.Types
import LLVM.Primitives

public export
ErasableToAnyPtr Context where
  erase (MkCtxt ref) = erase ref
  unerase = MkCtxt

public export
contextCreate : HasIO io => io Context
contextCreate = do ref <- primIO prim__contextCreate
                   pure $ MkCtxt ref

public export
contextDispose : HasIO io => Context -> io ()
contextDispose (MkCtxt ref) = primIO $ prim__contextDispose ref
