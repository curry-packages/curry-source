--- Tests for `Language.Curry.SourceCodeClassifier`.
--- 
--- @version January 2025

module TestSourceCodeClassifier where

import Language.Curry.SourceCodeClassifier ( getDeclarationsInModule )

import Test.Prop ( PropIO, (-=-), returns )

runTest :: IO Bool -> PropIO 
runTest = flip returns True

testModule :: PropIO
testModule = runTest $
  let ops     = [ ("myOp1",(18,21),(21,23)), ("myOp2",(24,26),(26,28))
                , ("myOp3",(0,0),(29,31)), (">?",(32,33),(33,35)) ]
      types   = [ ("MyData",(6,7),(7,8)), ("MyData2",(9,14),(14,15))
                , ("MyFunctionalType", (0,0), (16,17))]
      classes = [ ("ToInt",(36,37),(37,39)) ]
  in ((ops, types, classes) ==) <$> getDeclarationsInModule "Module"