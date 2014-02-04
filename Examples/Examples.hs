module Examples.Examples where

import Data
import DataIO

-- it is safe reading - we read only once
import System.IO.Unsafe

loadProgram :: FilePath -> Program
loadProgram path = read (unsafePerformIO (readFile path))

progString = loadProgram "Examples/string.sll"
progTree = loadProgram "Examples/tree.sll"
progList = loadProgram "Examples/list.sll"

callByValueTest :: Program
callByValueTest = loadProgram "Examples/callByValueTest.sll"

counters :: Program
counters = loadProgram "Examples/counters.sll"

progTest = loadProgram "Examples/test.sll"
