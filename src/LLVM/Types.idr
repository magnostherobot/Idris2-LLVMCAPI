module LLVM.Types

import Data.Vect
import Array

import LLVM.Primitives

public export
data Block = MkBlock BasicBlockRef

public export
data Value = MkValue ValueRef

public export
data Function = MkFunc ValueRef

public export
data Module = MkModule ModuleRef

public export
data Type' = MkType TypeRef

public export
data Context = MkCtxt ContextRef

-- TODO use this
public export
data Phi : List Block -> Type where
  MkPhi : ValueRef -> Phi bs

-- TODO https://llvm.org/doxygen/group__LLVMCCoreTypes.html
public export
data CallConv = CCC | FastCC | ColdCC | GHCCC

public export
callConv : CallConv -> Int
callConv cc = case cc of CCC => 0; FastCC => 8; ColdCC => 9; GHCCC => 10

public export
data IntPredicate = EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE

public export
intPred : IntPredicate -> Int
intPred EQ  = 32
intPred NE  = 33
intPred UGT = 34
intPred UGE = 35
intPred ULT = 36
intPred ULE = 37
intPred SGT = 38
intPred SGE = 39
intPred SLT = 40
intPred SLE = 41

-- TODO
public export
data CastMethod = IntToPtr -- ...

-- TODO
public export
castMethod : CastMethod -> Int

public export
Cast Bool Int where
  cast b = if b then 1 else 0

public export
functionType : HasIO io =>
               {argc : Nat} ->
               (ret : Type') ->
               (args : Vect argc Type') ->
               (variadic : Bool) ->
               io Type'
functionType ret args variadic = do
  let MkType ret = ret
  let args = map (\(MkType r) => r) args
  let variadic = cast variadic
  args <- toArray args
  let args = forgetArrType args
  let argc = cast argc
  let ref = prim__functionType ret args argc variadic
  pure $ MkType ref

public export
voidType : Type'
voidType = MkType prim__voidType

public export
intType : Nat -> Type'
intType = MkType . prim__intType . cast

-- TODO support address spaces
public export
pointerType : Type' -> Type'
pointerType (MkType t) = MkType $ prim__pointerType t 0

public export
sizeOf : Type' -> Value
sizeOf (MkType t) = MkValue $ prim__sizeOf t

public export
structCreateNamed : HasIO io =>
                    (context : Context) ->
                    (name : String) ->
                    io Type'
structCreateNamed (MkCtxt ctxt) name =
  pure $ MkType !(primIO $ prim__structCreateNamed ctxt name)

public export
structSetBody : HasIO io =>
                {n : _} ->
                (struct : Type') ->
                (members : Vect n Type') ->
                (packed : Bool) ->
                io ()
structSetBody (MkType t) xs packed = do
  let n = cast n
  let xs = map (\(MkType x) => x) xs
  xs <- toArray xs
  let packed = cast packed
  primIO $ prim__structSetBody t (forgetArrType xs) n packed
