module LLVM.Primitives

import Data.Array

public export
Opaque : String -> Type
Opaque x = AnyPtr

public export
ModuleRef : Type
ModuleRef = Opaque "LLVMModuleRef"

public export
ValueRef : Type
ValueRef = Opaque "LLVMValueRef"

public export
TypeRef : Type
TypeRef = Opaque "LLVMTypeRef"

public export
BuilderRef : Type
BuilderRef = Opaque "LLVMBuilderRef"

public export
BasicBlockRef : Type
BasicBlockRef = Opaque "LLVMBasicBlockRef"

public export
ContextRef : Type
ContextRef = Opaque "LLVMContextRef"

llvmext : String -> String
llvmext x = "C:" ++ x ++ ",libLLVM-10"

public export
%foreign (llvmext "LLVMSetInstructionCallConv")
prim__setInstructionCallConv : ValueRef -> Int -> PrimIO ()

public export
%foreign (llvmext "LLVMPositionBuilderAtEnd")
prim__positionBuilderAtEnd : BuilderRef -> BasicBlockRef -> PrimIO ()

public export
%foreign (llvmext "LLVMInt32Type")
prim__int32Type : TypeRef

public export
%foreign (llvmext "LLVMTypeOf")
prim__typeOf : ValueRef -> TypeRef

public export
%foreign (llvmext "LLVMWriteBitcodeToFile")
prim__writeBitcodeToFile : ModuleRef -> String -> PrimIO ()

public export
%foreign (llvmext "LLVMWriteBitcodeToMemoryBufffer")
prim__writeBitcodeToMemoryBuffer : ModuleRef -> PrimIO String

public export
%foreign (llvmext "LLVMIntType")
prim__intType : Int -> TypeRef

public export
%foreign (llvmext "LLVMBuildRet")
prim__buildRet : BuilderRef -> ValueRef -> PrimIO ()

public export
%foreign (llvmext "LLVMCreateBuilder")
prim__createBuilder : PrimIO BuilderRef

public export
%foreign (llvmext "LLVMStructSetBody")
prim__structSetBody : TypeRef -> Arr' -> Int -> Int -> PrimIO ()

public export
%foreign (llvmext "LLVMStructCreateNamed")
prim__structCreateNamed : ContextRef -> String -> PrimIO TypeRef

public export
%foreign (llvmext "LLVMConstInt")
prim__constInt : TypeRef -> Int -> Int -> ValueRef

public export
%foreign (llvmext "LLVMBuildLoad2")
prim__buildLoad : BuilderRef -> TypeRef -> ValueRef -> String ->
                  PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildStore")
prim__buildStore : BuilderRef -> ValueRef -> ValueRef -> PrimIO ValueRef

public export
%foreign (llvmext "LLVMAddIncoming")
prim__addIncoming : ValueRef -> Arr' -> Arr' -> Int -> PrimIO ()

public export
%foreign (llvmext "LLVMBuildPhi")
prim__buildPhi : BuilderRef -> TypeRef -> String -> PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildPointerCast")
prim__buildPointerCast : BuilderRef -> ValueRef -> ValueRef -> String ->
                         PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildCast")
prim__buildCast : BuilderRef -> Int -> ValueRef -> TypeRef -> String ->
                  PrimIO ValueRef

public export
%foreign (llvmext "LLVMAppendBasicBlock")
prim__appendBasicBlock : ValueRef -> String -> PrimIO BasicBlockRef

public export
%foreign (llvmext "LLVMPointerType")
prim__pointerType : TypeRef -> Int -> TypeRef

public export
%foreign (llvmext "LLVMSizeOf")
prim__sizeOf : TypeRef -> ValueRef

public export
%foreign (llvmext "LLVMBuildICmp")
prim__buildICmp : BuilderRef -> Int -> ValueRef -> ValueRef ->
                  String -> PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildBr")
prim__buildBr : BuilderRef -> BasicBlockRef -> PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildCondBr")
prim__buildCondBr : BuilderRef ->
                    (if' : ValueRef) ->
                    (then' : BasicBlockRef) ->
                    (else' : BasicBlockRef) ->
                    PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildCall2")
prim__buildCall : BuilderRef -> TypeRef -> ValueRef -> Arr' -> Int -> String ->
                  PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildSwitch")
prim__buildSwitch : BuilderRef -> ValueRef -> BasicBlockRef -> Int ->
                    PrimIO ValueRef

public export
%foreign (llvmext "LLVMAddCase")
prim__addCase : ValueRef -> ValueRef -> BasicBlockRef -> PrimIO ()

public export
%foreign (llvmext "LLVMFunctionType")
prim__functionType : (ret : TypeRef) ->
                     (args : Arr') ->
                     (argc : Int) ->
                     (variadic : Int) ->
                     TypeRef

public export
%foreign (llvmext "LLVMModuleCreateWithName")
prim__createModuleWithName : String -> PrimIO ModuleRef

public export
%foreign (llvmext "LLVMDisposeModule")
prim__disposeModule : ModuleRef -> PrimIO ()

public export
%foreign (llvmext "LLVMAddFunction")
prim__addFunction : ModuleRef -> String -> TypeRef -> PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildGlobalString")
prim__buildGlobalString : BuilderRef ->
                          (value : String) ->
                          (name : String) ->
                          PrimIO ValueRef

public export
%foreign (llvmext "LLVMBuildGlobalString")
prim__buildGlobalStringPtr : BuilderRef ->
                             (value : String) ->
                             (name : String) ->
                             PrimIO ValueRef

public export
%foreign (llvmext "LLVMVoidType")
prim__voidType : TypeRef

public export
%foreign (llvmext "LLVMDisposeBuilder")
prim__disposeBuilder : BuilderRef -> PrimIO ()

public export
%foreign (llvmext "LLVMContextCreate")
prim__contextCreate : PrimIO ContextRef

public export
%foreign (llvmext "LLVMContextDispose")
prim__contextDispose : ContextRef -> PrimIO ()
