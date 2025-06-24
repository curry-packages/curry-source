------------------------------------------------------------------------------
--- This module provides functions to extract span information of entities
--- defined in Curry source programs. These functions extract the
--- span information of operations, types, and classes. The span information
--- consists of the comment span and the source code span of each entity.
---
--- @version June 2025
------------------------------------------------------------------------------

module Language.Curry.SourceCodeClassifier
  ( getDeclarationsInModule, getOperationsInModule
  , getTypesInModule, getClassesInModule
  ) where

import Data.Maybe ( catMaybes )
import Data.List  ( sortBy )

import Curry.Comment  ( readComments )
import Curry.Files    ( readFullAST, readShortAST )
import Curry.Types    ( Module(..), Decl(..) )
import Curry.Ident    ( idName )
import Curry.SpanInfo ( SpanInfo(..) )
import Curry.Span     ( Span(..) )
import Curry.Position ( Position (..) )

import Data.Trie as T ( Trie, empty, insert, lookup )

--- A line span is a pair of a start and an end line number.
--- The start line number is inclusive, the end line number is exclusive.
type LineSpan = (Int, Int)

--- Denotes a missing span. This is used for occurrences of entites without
--- a comment, e.g., the comment span is non-existent.
missing :: LineSpan
missing = (0, 0)

--- An occurrence of some entity in the source code consists of
--- the entity's name, the comment span, and the source code span.
type Occurrence = (String, LineSpan, LineSpan)

--- Extracts all operations, type declarations, and class declarations
--- in a module with their associated comment and source code spans.
getDeclarationsInModule :: String -> IO ([Occurrence],[Occurrence],[Occurrence])
getDeclarationsInModule mn = do
  (mdl, comments) <- readModule mn

  let ops     = collectOperationsInModule mdl comments
  let types   = collectTypesInModule      mdl comments
  let classes = collectClassesInModule    mdl comments

  return (ops, types, classes)

--- Extracts all operations in a module with their comment and source code spans.
getOperationsInModule :: String -> IO [Occurrence]
getOperationsInModule mn =
  uncurry collectOperationsInModule <$> readModule mn

--- Extracts all types in a module with their comment and source code spans.
getTypesInModule :: String -> IO [Occurrence]
getTypesInModule mn =
  uncurry collectTypesInModule <$> readModule mn

--- Extracts all classes in a module with their comment and source code spans.
getClassesInModule :: String -> IO [Occurrence]
getClassesInModule mn =
  uncurry collectClassesInModule <$> readModule mn

--------------------------------------------------------------------------------
-- Implementation of the source code classifier.

--- Some entity with a line span.
type Entity a = (a, LineSpan)

--- A signature of some operation or operations in the source code,
--- consisting of the name(s) and the code span.
---
--- Because, e.g.,
---  > x,y,z :: Int -> Int
--- is a valid Curry signature, we need to be able to associate
--- multiple names with a single signature span.
type SignatureE = Entity [String]

--- A type declaration in the source code.
type TypeDeclE = Entity String

--- A class declaration
type ClassDeclE = Entity String

--- Given some module and comment line spans, this function collects all
--- operations in the module with their associated comment and code spans.
collectOperationsInModule :: Module a -> [LineSpan] -> [Occurrence]
collectOperationsInModule mdl comments =
  let sigs = collectSignatures mdl
      ops  = collectOperations mdl
  in extendWithRules ops $ addComments createOccs comments sigs
 where
  -- Creates occurrences from the signature and the comment span of
  -- an operation or multiple operations.
  createOccs :: LineSpan -> SignatureE -> [Occurrence]
  createOccs cls (is, sigSpan) = [(i, cls, sigSpan) | i <- is]

  collectSignatures :: Module a -> [SignatureE]
  collectSignatures (Module _ _ _ _ _ _ ops)
    = sortBy lineNumber $ catMaybes $ map collectSignature ops

  -- Collects the identifier and span of all operation signatures.
  collectSignature :: Decl a -> Maybe SignatureE
  collectSignature decl = case decl of
    (TypeSig si is _) -> do
      sp <- getLineSpan si
      return (map idName is, sp)
    _ -> Nothing

  -- Collects the span lines of all operations (rules) and stores them
  -- in a trie.
  collectOperations :: Module a -> T.Trie LineSpan
  collectOperations (Module _ _ _ _ _ _ ops) = foldr collect T.empty ops
   where
    collect :: Decl a -> Trie LineSpan -> Trie LineSpan
    collect decl trie = case decl of
      (FunctionDecl si _ i _) -> case getLineSpan si of
        Nothing -> trie
        Just ls -> T.insert (idName i) ls trie
      _ -> trie

  -- Extends the occurrences with the code spans of the operations'
  -- rule definitions.
  extendWithRules :: T.Trie LineSpan -> [Occurrence] -> [Occurrence]
  extendWithRules ops = map extend
   where
    extend :: Occurrence -> Occurrence
    extend occ@(i, cSpan, sigSpan) = case T.lookup i ops of
      Nothing -> occ
      Just opSpan -> (i, cSpan, extendSpan sigSpan opSpan)

    extendSpan :: LineSpan -> LineSpan -> LineSpan
    extendSpan (s1, e1) (s2, e2) = (min s1 s2, max e1 e2)

--- Given some module and comment line spans, this function collects all
--- type declarations in the module with their associated comment and
--- code spans.
collectTypesInModule :: Module a -> [LineSpan] -> [Occurrence]
collectTypesInModule mdl comments =
  let types = collectTypes mdl
  in addComments createOcc comments types
 where
  collectTypes :: Module a -> [TypeDeclE]
  collectTypes (Module _ _ _ _ _ _ decls)
    = sortBy lineNumber $ catMaybes $ map collectType decls

  collectType :: Decl a -> Maybe TypeDeclE
  collectType decl = case decl of
    (DataDecl si i _ _ _)    -> typeDeclE si i
    (NewtypeDecl si i _ _ _) -> typeDeclE si i
    (TypeDecl si i _ _)      -> typeDeclE si i
    _ -> Nothing

  -- Collects the identifier and span of some type declaration.
  typeDeclE si i = do
      sp <- getLineSpan si
      return (idName i, sp)

--- Given some module and comment line spans, this function collects all
--- classes in the module with their associated comment and code spans.
collectClassesInModule :: Module a -> [LineSpan] -> [Occurrence]
collectClassesInModule mdl comments =
  addComments createOcc comments $ collectClasses mdl
 where
  collectClasses :: Module a -> [ClassDeclE]
  collectClasses (Module _ _ _ _ _ _ decls)
    = sortBy lineNumber $ catMaybes $ map collectClass decls

  collectClass :: Decl a -> Maybe (ClassDeclE)
  collectClass decl = case decl of
    (ClassDecl si _ _ i _ _ _) -> do
      sp <- getLineSpan si
      return (idName i, sp)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Various helper functions.

--- Reads and returns the short AST and comments of a module.
readModule :: String -> IO (Module (), [LineSpan])
readModule mn = do
  comments <- (mergeCommentSpans . getCommentSpans) <$> readComments mn
  mdl <- readShortAST mn
  return (mdl, comments)

-- Creates a single occurrence from an entity that holds one
-- name and a line span, with some associated comment span.
createOcc :: LineSpan -> Entity String -> [Occurrence]
createOcc cls (i, sigSpan) = [(i, cls, sigSpan)]

-- Assigns the comment spans to some entities.
--
-- Notice that because the comment line positions as well as the entity
-- line positions are steadily increasing, we can merge in linear time in the
-- number of comments and signatures.
--
-- For `addComments f cs ss`,
--  - `f` is a function that creates occurrences from a comment span and
--    some entity that has a line span
--  - `cs` is the list of comment spans
--  - `ss` is the list of signature spans
addComments :: (LineSpan -> (a, LineSpan) -> [Occurrence]) -> [LineSpan]
            -> [(a, LineSpan)] -> [Occurrence]
addComments f cs ss = case (cs, ss) of
  ([], _) -> concatMap (f missing) ss
  (_, []) -> []
  (c:cs', s@(_, sigSpan):ss')
    -- Skip unconnected comments:
    | end c <  start sigSpan -> addComments f cs' (s:ss')
    -- Add with connected comments:
    | end c == start sigSpan -> f c s       ++ addComments f cs' ss'
    -- Add with missing comments:
    | otherwise              -> f missing s ++ addComments f (c:cs') ss'

--- Converts all comments with span information to line spans.
getCommentSpans :: [(Span, _)] -> [LineSpan]
getCommentSpans = sortBy startLine . catMaybes . map (getLineSpan . fst)
 where
  startLine :: LineSpan -> LineSpan -> Bool
  startLine (x, _) (y, _) = x <= y

--- Merges adjacent line spans of comments.
---
--- Consider the spans `[(1, 2), (2, 3), (4, 5)]`. The first two spans are
--- adjacent and should be merged to `(1, 3)`. The last span is not adjacent
--- to any other span and should be left as is.
mergeCommentSpans :: [LineSpan] -> [LineSpan]
mergeCommentSpans = foldr merge []
 where
  merge :: LineSpan -> [LineSpan] -> [LineSpan]
  merge sl [] = [sl]
  merge sl (sr : xs)
    | end sl == start sr = (start sl, end sr) : xs
    | otherwise          = sl : sr : xs

-- Returns the start line number of a line span.
start :: LineSpan -> Int
start = fst

-- Returns the end line number of a line span.
--
-- The end line number is exclusive, i.e., the line span `(s, e)` covers
-- the lines `s, s+1, ..., e-1`.
end :: LineSpan -> Int
end = snd

--- Orders entities by their start line number.
lineNumber :: Entity a -> Entity a -> Bool
lineNumber (_, (s1, _)) (_, (s2, _)) = s1 <= s2

--- Extracts the row number from a position.
row :: Position -> Maybe Int
row (Position r _) = Just r
row NoPos          = Nothing

class HasLineSpan a where
  getLineSpan :: a -> Maybe LineSpan

instance HasLineSpan Span where
  getLineSpan NoSpan     = Nothing
  getLineSpan (Span s e) = do
    sp <- row s
    ep <- row e
    return (sp, ep + 1)

instance HasLineSpan SpanInfo where
  getLineSpan (SpanInfo s _) = getLineSpan s
  getLineSpan NoSpanInfo     = Nothing