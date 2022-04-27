module LLVM.Block

import Data.Fin
import Array

import LLVM.Types
import LLVM.Primitives

public export
ErasableToAnyPtr Block where
  erase (MkBlock ref) = erase ref
  unerase ref = MkBlock ref

public export
appendBlock : HasIO io => Function -> String -> io Block
appendBlock (MkFunc fref) name =
  pure $ MkBlock !(primIO $ prim__appendBasicBlock fref name)
