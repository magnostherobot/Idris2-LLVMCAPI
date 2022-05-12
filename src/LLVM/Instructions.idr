module LLVM.Instructions

import Control.Linear.LIO
import Data.Vect
import Data.Array

import LLVM.Block
import LLVM.Builder
import LLVM.Primitives
import LLVM.Types
import LLVM.Value

public export
data BPair : Type -> Type -> Type where
  B : (1 x : t) -> (y : u) -> BPair t u

bval : LinearIO io => BuilderRef -> ValueRef -> L1 io $ BPair (Builder _) Value
bval x y = pure1 $ B (MkBuilder x) (MkValue y)

public export
buildRet : LinearIO io =>
           (1 b : Builder (Just bl)) ->
           Value ->
           L1 io (BPair (Builder (Just bl)) ())
buildRet (MkBuilder ref) (MkValue v) = do primIO $ prim__buildRet ref v
                                          pure1 $ B (MkBuilder ref) ()

public export
buildLoad : LinearIO io =>
            (1 b : Builder (Just bl)) ->
            Type' ->
            Value ->
            String ->
            L1 io $ BPair (Builder (Just bl)) Value
buildLoad (MkBuilder ref) (MkType t) (MkValue v) name = do
  res <- primIO $ prim__buildLoad ref t v name
  bval ref res

public export
buildStore : LinearIO io =>
             (1 b : Builder (Just bl)) ->
             (value : Value) ->
             (ptr : Value) ->
             L1 io $ BPair (Builder (Just bl)) Value
buildStore (MkBuilder ref) (MkValue v) (MkValue p) = do
  res <- primIO $ prim__buildStore ref v p
  bval ref res

-- TODO
-- could ensure that phis are only built as the first instruction in a block
public export
buildPhi : LinearIO io =>
           (1 b : Builder (Just bl)) ->
           Type' ->
           String ->
           L1 io $ BPair (Builder (Just bl)) Value
buildPhi (MkBuilder ref) (MkType t) name = do
  res <- primIO $ prim__buildPhi ref t name
  bval ref res

public export
addIncoming : LinearIO io =>
              {n : _} ->
              Value ->
              Vect n (Value, Block) ->
              L io ()
addIncoming (MkValue phi) vsbs = do
  let (vs, bs) = unzip vsbs
  MkArr vs <- toArray vs
  MkArr bs <- toArray bs
  primIO $ prim__addIncoming phi vs bs (cast n)
  pure ()

public export
buildPhiWithIncoming : LinearIO io =>
                       {n : _} ->
                       (1 b : Builder (Just bl)) ->
                       Type' ->
                       Vect n (Value, Block) ->
                       String ->
                       L1 io $ BPair (Builder (Just bl)) Value
buildPhiWithIncoming b t vsbs name = do
  B b phi <- buildPhi b t name
  addIncoming phi vsbs
  pure1 $ B b phi

public export
buildCall : {argc : Nat} ->
            LinearIO io =>
            (1 builder : Builder (Just bl)) ->
            (funcType : Type') ->
            (func : Function) ->
            (args : Vect argc Value) ->
            (name : String) ->
            L1 io $ BPair (Builder (Just bl)) Value 
buildCall (MkBuilder b) (MkType t) (MkFunc f) args name = do
  MkArr args <- toArray args
  let argc = cast argc
  res <- primIO $ prim__buildCall b t f args argc name
  bval b res

-- TODO not sure what "value" is returned here
public export
buildBr : LinearIO io =>
          (1 builder : Builder (Just bl)) ->
          (target : Block) ->
          L1 io $ BPair (Builder (Just bl)) Value
buildBr (MkBuilder b) (MkBlock x) =
  bval b !(primIO $ prim__buildBr b x)

public export
buildCondBr : LinearIO io =>
              (1 builder : Builder (Just bl)) ->
              (condition : Value) ->
              (then' : Block) ->
              (else' : Block) ->
              L1 io $ BPair (Builder (Just bl)) Value
buildCondBr (MkBuilder b) (MkValue c) (MkBlock t) (MkBlock e) =
  bval b !(primIO $ prim__buildCondBr b c t e)

public export
buildICmp : LinearIO io =>
            (1 builder : Builder (Just bl)) ->
            (pred : IntPredicate) ->
            (x, y : Value) ->
            (name : String) ->
            L1 io $ BPair (Builder (Just bl)) Value
buildICmp (MkBuilder b) pred (MkValue x) (MkValue y) name = do
  let pred = intPred pred
  res <- primIO $ prim__buildICmp b pred x y name
  bval b res

public export
buildCast : LinearIO io =>
            (1 builder : Builder (Just bl)) ->
            (method : CastMethod) ->
            (x : Value) ->
            (targetType : Type') ->
            (name : String) ->
            L1 io $ BPair (Builder (Just bl)) Value
buildCast (MkBuilder b) method (MkValue v) (MkType t) name = do
  let method = castMethod method
  res <- primIO $ prim__buildCast b method v t name
  bval b res

public export
buildPointerCast : LinearIO io =>
                   (1 builder : Builder (Just bl)) ->
                   (x, y : Value) ->
                   (name : String) ->
                   L1 io $ BPair (Builder (Just bl)) Value
buildPointerCast (MkBuilder b) (MkValue x) (MkValue y) name =
  bval b !(primIO $ prim__buildPointerCast b x y name)

public export
buildGlobalString : LinearIO io =>
                    (1 builder : Builder (Just bl)) ->
                    (value : String) ->
                    (name : String) ->
                    L1 io $ BPair (Builder (Just bl)) Value
buildGlobalString (MkBuilder b) value name =
  bval b !(primIO $ prim__buildGlobalString b value name)

public export
buildGlobalStringPtr : LinearIO io =>
                       (1 builder : Builder (Just bl)) ->
                       (value : String) ->
                       (name : String) ->
                       L1 io $ BPair (Builder (Just bl)) Value
buildGlobalStringPtr (MkBuilder b) value name =
  bval b !(primIO $ prim__buildGlobalStringPtr b value name)

public export
setInstructionCallConv : HasIO io => Function -> CallConv -> io ()
setInstructionCallConv (MkFunc f) cc =
  primIO $ prim__setInstructionCallConv f (callConv cc)
