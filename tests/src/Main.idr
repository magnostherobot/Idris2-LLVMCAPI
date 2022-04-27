import Test.Unit
import Control.App

import LLVMTest

main : IO ()
main = runTests LLVMTest.tests
