module LLVM.Context

import Control.Linear.LIO
import Data.Array

import LLVM.Types
import LLVM.Primitives

public export
ErasableToAnyPtr Context where
  erase (MkCtxt ref) = erase ref
  unerase = MkCtxt

public export
contextCreate : LinearIO io => L1 io Context
contextCreate = do ref <- primIO prim__contextCreate
                   pure1 $ MkCtxt ref

public export
contextDispose : HasIO io => (1 ctxt : Context) -> io ()
contextDispose (MkCtxt ref) = primIO $ prim__contextDispose ref
