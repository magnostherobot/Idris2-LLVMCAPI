module LLVM.Types

import Data.Vect
import Data.Array

import Control.Linear.LIO

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

infixr 5 <#

public export
data LLPair : Type -> Type -> Type where
  (<#) : (1 l : a) -> (r : b) -> LLPair a b

public export
ContextWith : Type -> Type
ContextWith = LLPair Context

public export
functionType : LinearIO io =>
               {argc : Nat} ->
               (ret : Type') ->
               (args : Vect argc Type') ->
               (variadic : Bool) ->
               L io Type'
functionType ret args variadic = do
  let MkType ret = ret
  let args = map (\(MkType r) => r) args
  let variadic = cast variadic
  MkArr args <- toArray args
  let argc = cast argc
  let ref = prim__functionType ret args argc variadic

  -- TODO
  --
  -- Part of the point of linear types is that it's hard to avoid cleaning them
  -- up after they've been useful, but passing them to foreign functions
  -- involves erasing their linearity. Is there a better way of doing this,
  -- that would raise a compiler error if the following cleanup line is
  -- omitted?
  freeArray (MkArr args) {t = Type'} {n = cast argc}
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
typeOf : Value -> Type'
typeOf (MkValue v) = MkType $ prim__typeOf v

public export
structCreateNamed : LinearIO io =>
                    (1 context : Context) ->
                    (name : String) ->
                    L1 io (ContextWith Type')
structCreateNamed (MkCtxt ctxt) name = do
  t <- primIO $ prim__structCreateNamed ctxt name
  pure1 (MkCtxt ctxt <# MkType t)

public export
structSetBody : LinearIO io =>
                {n : _} ->
                (struct : Type') ->
                (members : Vect n Type') ->
                (packed : Bool) ->
                L io ()
structSetBody (MkType t) xs packed = do
  let n = cast n
  let xs = map (\(MkType x) => x) xs
  MkArr xs <- toArray xs
  let packed = cast packed
  freeArray (MkArr xs) {t = TypeRef} {n = cast n}
  primIO $ prim__structSetBody t xs n packed
