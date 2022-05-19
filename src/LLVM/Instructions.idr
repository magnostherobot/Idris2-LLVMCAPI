module LLVM.Instructions

import Control.Linear.LIO
import Data.Vect
import Data.Array

import LLVM.Block
import LLVM.Builder
import LLVM.Primitives
import LLVM.Types
import LLVM.Value

infixr 5 <#

{- TODO
 -
 - Currently this type requires that the block be in the type of the function,
 - which is an issue because new blocks might be built and used within said
 - function. Having the block be a member of this record, though, would mean
 - that we couldn't make assertions about the builder's state changing/remaining
 - the same (i.e. if the builder leaves the function call in the same block as
 - it entered it, it might be nice to have that information).
 -}
public export
record BuildResult mb t where
  constructor Result
  1 builder : Builder mb
  value : t

public export
BuildResultAt : Block -> Type -> Type
BuildResultAt block = BuildResult (Just block)

bres : LinearIO io => BuilderRef -> ValueRef ->
       L1 io $ BuildResultAt block Value

public export
buildRet : LinearIO io =>
           (1 b : BuilderAt block) ->
           Value ->
           L1 io (BuildResultAt block ())
buildRet (MkBuilder ref) (MkValue v) = do
  primIO $ prim__buildRet ref v
  pure1 $ Result (MkBuilder ref) ()

-- -- TODO
public export
buildRetVoid : LinearIO io =>
               (1 b : BuilderAt block) ->
               L1 io (BuildResultAt block ())

public export
buildLoad : LinearIO io =>
            (1 b : BuilderAt block) ->
            Type' ->
            Value ->
            String ->
            L1 io (BuildResultAt block Value)
buildLoad (MkBuilder ref) (MkType t) (MkValue v) name = do
  res <- primIO $ prim__buildLoad ref t v name
  bres ref res

public export
buildStore : LinearIO io =>
             (1 b : BuilderAt block) ->
             (value : Value) ->
             (ptr : Value) ->
             L1 io (BuildResultAt block Value)
buildStore (MkBuilder ref) (MkValue v) (MkValue p) = do
  res <- primIO $ prim__buildStore ref v p
  bres ref res

-- TODO
-- could ensure that phis are only built as the first instruction in a block
public export
buildPhi : LinearIO io =>
           (1 b : BuilderAt block) ->
           Type' ->
           String ->
           L1 io (BuildResultAt block Value)
buildPhi (MkBuilder ref) (MkType t) name = do
  res <- primIO $ prim__buildPhi ref t name
  bres ref res

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
                       (1 b : BuilderAt block) ->
                       Type' ->
                       Vect n (Value, Block) ->
                       String ->
                       L1 io (BuildResultAt block Value)
buildPhiWithIncoming b t vsbs name = do
  Result b phi <- buildPhi b t name
  addIncoming phi vsbs
  pure1 (Result b phi)

public export
buildCall : {argc : Nat} ->
            LinearIO io =>
            (1 builder : BuilderAt block) ->
            (funcType : Type') ->
            (func : Function) ->
            (args : Vect argc Value) ->
            (name : String) ->
            L1 io (BuildResultAt block Value)
buildCall (MkBuilder b) (MkType t) (MkFunc f) args name = do
  MkArr args <- toArray args
  let argc = cast argc
  res <- primIO $ prim__buildCall b t f args argc name
  bres b res

-- TODO not sure what "value" is returned here
public export
buildBr : LinearIO io =>
          (1 builder : BuilderAt block) ->
          (target : Block) ->
          L1 io (BuildResultAt block Value)
buildBr (MkBuilder b) (MkBlock x) =
  bres b !(primIO $ prim__buildBr b x)

public export
buildCondBr : LinearIO io =>
              (1 builder : BuilderAt block) ->
              (condition : Value) ->
              (then' : Block) ->
              (else' : Block) ->
              L1 io (BuildResultAt block Value)
buildCondBr (MkBuilder b) (MkValue c) (MkBlock t) (MkBlock e) =
  bres b !(primIO $ prim__buildCondBr b c t e)

public export
buildICmp : LinearIO io =>
            (1 builder : BuilderAt block) ->
            (pred : IntPredicate) ->
            (x, y : Value) ->
            (name : String) ->
            L1 io (BuildResultAt block Value)
buildICmp (MkBuilder b) pred (MkValue x) (MkValue y) name = do
  let pred = intPred pred
  res <- primIO $ prim__buildICmp b pred x y name
  bres b res

public export
buildCast : LinearIO io =>
            (1 builder : BuilderAt block) ->
            (method : CastMethod) ->
            (x : Value) ->
            (targetType : Type') ->
            (name : String) ->
            L1 io (BuildResultAt block Value)
buildCast (MkBuilder b) method (MkValue v) (MkType t) name = do
  let method = castMethod method
  res <- primIO $ prim__buildCast b method v t name
  bres b res

public export
buildPointerCast : LinearIO io =>
                   (1 builder : BuilderAt block) ->
                   (x : Value) ->
                   (t : Type') ->
                   (name : String) ->
                   L1 io (BuildResultAt block Value)
buildPointerCast (MkBuilder b) (MkValue x) (MkType t) name =
  bres b !(primIO $ prim__buildPointerCast b x t name)

public export
buildSwitch : LinearIO io =>
              (1 builder : BuilderAt block) ->
              Value ->
              (defaultBlock : Block) ->
              (numCases : Nat) ->
              L1 io (BuildResultAt block Value)
buildSwitch (MkBuilder b) (MkValue x) (MkBlock d) n =
  bres b !(primIO $ prim__buildSwitch b x d (cast n))

-- TODO
-- Could ensure that all switch statements are given all of their branches
public export
addCase : HasIO io =>
          (switch : Value) ->
          (match : Value) ->
          (destination : Block) ->
          io ()
addCase (MkValue s) (MkValue m) (MkBlock d) =
  primIO $ prim__addCase s m d

public export
buildGlobalString : LinearIO io =>
                    (1 builder : BuilderAt block) ->
                    (value : String) ->
                    (name : String) ->
                    L1 io (BuildResultAt block Value)
buildGlobalString (MkBuilder b) value name =
  bres b !(primIO $ prim__buildGlobalString b value name)

public export
buildGlobalStringPtr : LinearIO io =>
                       (1 builder : BuilderAt block) ->
                       (value : String) ->
                       (name : String) ->
                       L1 io (BuildResultAt block Value)
buildGlobalStringPtr (MkBuilder b) value name =
  bres b !(primIO $ prim__buildGlobalStringPtr b value name)

public export
setInstructionCallConv : HasIO io => Function -> CallConv -> io ()
setInstructionCallConv (MkFunc f) cc =
  primIO $ prim__setInstructionCallConv f (callConv cc)
