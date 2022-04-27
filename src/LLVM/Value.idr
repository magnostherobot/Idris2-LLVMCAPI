module LLVM.Value

import Data.Fin
import Array

import LLVM.Primitives
import LLVM.Types

public export
ErasableToAnyPtr Value where
  erase (MkValue ref) = erase ref
  unerase ref = MkValue ref

public export
Cast Function Value where
  cast (MkFunc ref) = MkValue ref

public export
Cast Value Function where
  cast (MkValue ref) = MkFunc ref

public export
addFunction : HasIO io => Module -> String -> Type' -> io Function
addFunction (MkModule m) name (MkType t) =
  pure $ MkFunc !(primIO $ prim__addFunction m name t)

public export
constInt : Type' -> Integer -> Bool -> Value
constInt (MkType t) x signExtend =
  MkValue $ prim__constInt t (cast x) (cast signExtend)
