module LLVM.Value

import Control.Linear.LIO

import Data.Fin
import Data.Array

import LLVM.Primitives
import LLVM.Types

public export
data ModuleWith : t -> Type where
  M : (1 _ : Module) -> t -> ModuleWith t

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
addFunction : LinearIO io => (1 _ : Module) -> String -> Type' ->
              L1 io $ ModuleWith Function
addFunction (MkModule m) name (MkType t) =
  pure1 $ M (MkModule m) (MkFunc !(primIO $ prim__addFunction m name t))

public export
getParam : Function -> Nat -> Value
getParam (MkFunc f) i = MkValue $ prim__getParam f (cast i)

public export
constInt : Type' -> Integer -> Bool -> Value
constInt (MkType t) x signExtend =
  MkValue $ prim__constInt t (cast x) (cast signExtend)
