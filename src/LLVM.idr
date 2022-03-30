module LLVM

import System.FFI
import Data.Vect

import Array

llvmext : String -> String
llvmext x = "C:" ++ x ++ ",libLLVM-10"

Opaque : String -> Type
Opaque x = AnyPtr

ModuleRef     = Opaque "LLVMModuleRef"
ValueRef      = Opaque "LLVMValueRef"
TypeRef       = Opaque "LLVMTypeRef"
BuilderRef    = Opaque "LLVMBuilderRef"
BasicBlockRef = Opaque "LLVMBasicBlockRef"
ContextRef    = Opaque "LLVMContextRef"

%foreign (llvmext "LLVMModuleCreateWithName")
prim__createModuleWithName : String -> PrimIO ModuleRef

||| Creates a module with the given name.
createModuleWithName : HasIO io => String -> io ModuleRef
createModuleWithName n = primIO $ prim__createModuleWithName n

%foreign (llvmext "LLVMAddFunction")
prim__addFunction : ModuleRef -> String -> TypeRef -> PrimIO ValueRef

||| Adds a function to a module, and returns the new function.
addFunction : HasIO io => ModuleRef -> String -> TypeRef -> io ValueRef
addFunction m n t = primIO $ prim__addFunction m n t

%foreign (llvmext "LLVMVoidType")
voidType : TypeRef

Cast Bool Int where
  cast x = if x then 1 else 0

%foreign (llvmext "LLVMFunctionType")
prim__functionType : (ret : TypeRef) ->
                     (args : Arr') ->
                     (argc : Int) ->
                     (variadic : Int) ->
                     TypeRef

functionType : {argc : Nat} ->
               (ret : TypeRef) ->
               (args : Arr argc TypeRef) ->
               (variadic : Bool) ->
               TypeRef
functionType ret args variadic =
  let args' = forgetArrType args
  in  prim__functionType ret args' (cast argc) (cast variadic)

functionTypeVect : HasIO io =>
                   {argc : Nat} ->
                   (ret : TypeRef) ->
                   (args : Vect argc TypeRef) ->
                   (variadic : Bool) ->
                   io TypeRef
functionTypeVect ret args variadic =
  do args' <- toArray args
     pure $ functionType ret args' variadic

%foreign (llvmext "LLVMIntType")
prim__intType : Int -> TypeRef

intType : Nat -> TypeRef
intType w = prim__intType (cast w)

%foreign (llvmext "LLVMInt32Type")
prim__int32Type : TypeRef

%foreign (llvmext "LLVMTypeOf")
prim__typeOf : ValueRef -> TypeRef

%foreign (llvmext "LLVMWriteBitcodeToFile")
prim__writeBitcodeToFile : ModuleRef -> String -> PrimIO ()

writeBitcodeToFile : HasIO io => ModuleRef -> String -> io ()
writeBitcodeToFile m n = primIO $ prim__writeBitcodeToFile m n

%foreign (llvmext "LLVMPositionBuilderAtEnd")
prim__positionBuilderAtEnd : BuilderRef -> BasicBlockRef -> PrimIO ()

positionBuilderAtEnd : HasIO io => BuilderRef -> BasicBlockRef -> io ()
positionBuilderAtEnd builder block =
  primIO $ prim__positionBuilderAtEnd builder block

%foreign (llvmext "LLVMSetInstructionCallConv")
prim__setInstructionCallConv : ValueRef -> Int -> PrimIO ()

-- TODO https://llvm.org/doxygen/group__LLVMCCoreTypes.html
data CallConv = CCC | FastCC | ColdCC | GHCCC
 
callConv : CallConv -> Int
callConv cc = case cc of CCC => 0; FastCC => 8; ColdCC => 9; GHCCC => 10

setInstructionCallConv : HasIO io => ValueRef -> CallConv -> io ()
setInstructionCallConv f cc =
  primIO $ prim__setInstructionCallConv f (callConv cc)

%foreign (llvmext "LLVMBuildCall2")
prim__buildCall : BuilderRef -> TypeRef -> ValueRef -> Arr' -> Int -> String ->
                  PrimIO ValueRef

buildCall : {argc : Nat} ->
            HasIO io =>
            (builder : BuilderRef) ->
            (funcType : TypeRef) ->
            (func : ValueRef) ->
            (args : Arr argc ValueRef) ->
            (name : String) ->
            io ValueRef
buildCall builder funcType func args name =
  let
    args' = forgetArrType args
    argc' = cast argc
  in
    primIO $ prim__buildCall builder funcType func args' argc' name

%foreign (llvmext "LLVMBuildBr")
prim__buildBr : BuilderRef -> BasicBlockRef -> PrimIO ValueRef

buildBr : HasIO io => BuilderRef -> BasicBlockRef -> io ValueRef
buildBr builder target = primIO $ prim__buildBr builder target

%foreign (llvmext "LLVMBuildICmp")
prim__buildICmp : BuilderRef -> Int -> ValueRef -> ValueRef ->
                  String -> PrimIO ValueRef

data IntPredicate = EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE

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

buildICmp : HasIO io =>
            (builder : BuilderRef) ->
            (pred : IntPredicate) ->
            (x, y : ValueRef) ->
            (name : String) ->
            io ValueRef
buildICmp builder pred x y name =
  let
    pred' = intPred pred
  in
    primIO $ prim__buildICmp builder pred' x y name

%foreign (llvmext "LLVMSizeOf")
prim__sizeOf : TypeRef -> ValueRef

sizeOf : TypeRef -> ValueRef
sizeOf = prim__sizeOf

%foreign (llvmext "LLVMPointerType")
prim__pointerType : TypeRef -> Int -> TypeRef

-- TODO support address spaces
pointerType : TypeRef -> TypeRef
pointerType t = prim__pointerType t 0

%foreign (llvmext "LLVMAppendBasicBlock")
prim__appendBasicBlock : ValueRef -> String -> PrimIO BasicBlockRef

appendBasicBlock : HasIO io => ValueRef -> String -> io BasicBlockRef
appendBasicBlock f name = primIO $ prim__appendBasicBlock f name

-- TODO
data CastMethod = IntToPtr -- ...

-- TODO
castMethod : CastMethod -> Int

%foreign (llvmext "LLVMBuildCast")
prim__buildCast : BuilderRef -> Int -> ValueRef -> TypeRef -> String ->
                  PrimIO ValueRef

buildCast : HasIO io => BuilderRef -> CastMethod -> ValueRef -> TypeRef ->
            String -> io ValueRef
buildCast builder cm x t name =
  let
    cm' = castMethod cm
  in
    primIO $ prim__buildCast builder cm' x t name

%foreign (llvmext "LLVMBuildPointerCast")
prim__buildPointerCast : BuilderRef -> ValueRef -> ValueRef -> String ->
                         PrimIO ValueRef

buildPointerCast : HasIO io => BuilderRef -> (x, y : ValueRef) -> String ->
                   io ValueRef
buildPointerCast builder x y name =
  primIO $ prim__buildPointerCast builder x y name

%foreign (llvmext "LLVMBuildPhi")
prim__buildPhi : BuilderRef -> TypeRef -> String -> PrimIO ValueRef

buildPhi : HasIO io => BuilderRef -> TypeRef -> String -> io ValueRef
buildPhi builder t name = primIO $ prim__buildPhi builder t name

%foreign (llvmext "LLVMAddIncoming")
prim__addIncoming : ValueRef -> Arr' -> Arr' -> Int -> PrimIO ()

addIncoming' : HasIO io => {n : _} ->
              ValueRef -> Arr n ValueRef -> Arr n BasicBlockRef -> io ()
addIncoming' phi vs blocks =
  let
    vs' = forgetArrType vs
    blocks' = forgetArrType blocks
    n' = cast n
  in
    primIO $ prim__addIncoming phi vs' blocks' n'

addIncoming : HasIO io => {n : _} ->
              ValueRef -> Vect n (ValueRef, BasicBlockRef) -> io ()
addIncoming phi xs = do let (vs, blocks) = unzip xs
                        vs' <- toArray vs
                        blocks' <- toArray blocks
                        addIncoming' phi vs' blocks'

%foreign (llvmext "LLVMBuildLoad2")
prim__buildLoad : BuilderRef -> TypeRef -> ValueRef -> String ->
                  PrimIO ValueRef

buildLoad : HasIO io => BuilderRef -> TypeRef -> ValueRef -> String ->
            io ValueRef
buildLoad builder t x name = primIO $ prim__buildLoad builder t x name

%foreign (llvmext "LLVMConstInt")
prim__constInt : TypeRef -> Int -> Int -> ValueRef

constInt : TypeRef -> Integer -> Bool -> ValueRef
constInt t x signExtend = prim__constInt t (cast x) (cast signExtend)

%foreign (llvmext "LLVMStructCreateNamed")
prim__structCreateNamed : ContextRef -> String -> PrimIO TypeRef

structCreateNamed : HasIO io => ContextRef -> String -> io TypeRef
structCreateNamed ctxt name = primIO $ prim__structCreateNamed ctxt name

%foreign (llvmext "LLVMStructSetBody")
prim__structSetBody : TypeRef -> Arr' -> Int -> Int -> PrimIO ()

structSetBody' : HasIO io => {n : _} ->
                TypeRef -> Arr n TypeRef -> Bool -> io ()
structSetBody' struct members packed =
  let
    members' = forgetArrType members
    packed' = cast packed
    n' = cast n
  in
    primIO $ prim__structSetBody struct members' n' packed'

%foreign (llvmext "LLVMCreateBuilder")
prim__createBuilder : PrimIO BuilderRef

createBuilder : HasIO io => io BuilderRef
createBuilder = primIO prim__createBuilder

%foreign (llvmext "LLVMBuildRet")
prim__buildRet : BuilderRef -> ValueRef -> PrimIO ()

buildRet : HasIO io => BuilderRef -> ValueRef -> io ()
buildRet builder val = primIO $ prim__buildRet builder val

main : IO ()
main = do mod <- createModuleWithName "test_module"
          let int = intType 32
          funcT <- functionTypeVect int [int, int] False
          func <- addFunction mod "test_function" funcT
          block <- appendBasicBlock func "main"
          builder <- createBuilder
          positionBuilderAtEnd builder block
          let four = constInt int 4 False
          buildRet builder four
          writeBitcodeToFile mod "test.bc"
          pure ()
