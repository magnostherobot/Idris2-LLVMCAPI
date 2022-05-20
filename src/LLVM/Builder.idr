module LLVM.Builder

import Data.Maybe
import Control.Linear.LIO

import LLVM.Primitives
import LLVM.Types
import LLVM.Block

public export
data Builder : Maybe Block -> Type where
  MkBuilder : BuilderRef -> Builder b

OnBlock : {b : _} -> Builder b -> Type
OnBlock _ = IsJust b

public export
BuilderAt : Block -> Type
BuilderAt block = Builder (Just block)

public export
createBuilder : LinearIO io => L1 io (Builder Nothing)
createBuilder = do ref <- primIO prim__createBuilder
                   pure1 (MkBuilder ref)

public export
disposeBuilder : HasIO io => (1 b : Builder mb) -> io ()
disposeBuilder (MkBuilder ref) = primIO $ prim__disposeBuilder ref

public export
withBuilder : LinearIO io => {bl : Maybe Block} ->
              (1 f : (1 b : Builder Nothing) -> L1 io (Builder bl)) -> L io ()
withBuilder f = do b <- createBuilder
                   b <- f b
                   disposeBuilder b

public export
positionBuilderAtEnd : LinearIO io => {old : Maybe Block} ->
                       (1 b : Builder old) -> (block : Block) ->
                       L1 io (Builder (Just block))
positionBuilderAtEnd (MkBuilder builderRef) (MkBlock blockRef) = do
  primIO $ prim__positionBuilderAtEnd builderRef blockRef
  pure1 $ MkBuilder builderRef
