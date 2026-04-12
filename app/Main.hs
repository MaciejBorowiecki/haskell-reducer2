module Main where

import qualified Data.Map as Map
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment

data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String
newtype Prog = Prog {progDefs :: [Def]}
type DefMap = Map.Map Name Def

instance Show Expr where
  showsPrec _ (Var n) = showString n
  showsPrec p (e1 :$ e2) = showParen (p > 10) (showsPrec 10 e1 . showString " " . showsPrec 11 e2)

instance Show Def where
  show (Def name params expr) =
    unwords (name : params) ++ " = " ++ show expr

instance Show Prog where
  show (Prog defs) = unlines [show d | d <- defs]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [f] -> do
      code <- readFile f
      let prog = fromHsString code
          dmap = buildDefMap prog
          defs = progDefs prog
      case validateProgram defs dmap of
        Just errMsg -> putStrLn errMsg
        Nothing -> do
          putStr (show prog)
          putStrLn "------------------------------------------------------------"
          case Map.lookup "main" dmap of
            Just (Def _ _ body) -> printPath dmap body
            Nothing -> error "No `main` combinator given."

    _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: zadanie2 [--help] [file]"
  putStrLn "--help\t- display this message"
  putStrLn "file\t- file with program to reduce"

-- parser utils

fromHsString :: String -> Prog
fromHsString code = Prog (fromParseResult (parseModule code))

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseOk moduleTree) = fromHsModule moduleTree
fromParseResult (ParseFailed _ msg) = error $ "syntax error while parsing " ++ msg

-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]
fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) = map fromHsDecl decls

-- HsFunBind (HsMatch SrcLoc HsName [HsPat] HsRhs [HsDecl])
fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind [HsMatch _ (HsIdent name) hsArgs (HsUnGuardedRhs hsExpr) _]) =
  Def name (map fromHsArg hsArgs) (fromHsExpr hsExpr)
fromHsDecl (HsPatBind _ (HsPApp (UnQual (HsIdent name)) hsArgs) (HsUnGuardedRhs hsExpr) _) =
  Def name (map fromHsArg hsArgs) (fromHsExpr hsExpr)
fromHsDecl (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs hsExpr) _) =
  Def name [] (fromHsExpr hsExpr)
fromHsDecl (HsFunBind (name:_:_))
  = error ("Error: Multiple definitions for the same combinator " ++ show name)
fromHsDecl _ = error "Unexpected fromHsDecl error."

fromHsArg :: HsPat -> Pat
fromHsArg (HsPVar (HsIdent name)) = name
fromHsArg (HsPApp (UnQual (HsIdent name)) []) = name
fromHsArg (HsPParen inter) = fromHsArg inter
fromHsArg _ = error "Unexpected fromHsArg error."

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Var name
fromHsExpr (HsApp e1 e2) = fromHsExpr e1 :$ fromHsExpr e2
fromHsExpr (HsParen e) = fromHsExpr e
fromHsExpr _ = error "Unexpected fromHsExpr error."

insertDefMapHelper :: Def -> DefMap -> DefMap
insertDefMapHelper (Def name args expr) dmap = Map.insert name (Def name args expr) dmap

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = foldr insertDefMapHelper Map.empty defs

-- name conflicts

-- list of all names in expression
allVars :: Expr -> [Name]
allVars (Var n) = [n]
allVars (e1 :$ e2) = allVars e1 ++ allVars e2

allVarsList :: [Expr] -> [Name]
allVarsList [] = []
allVarsList (e : es) = allVars e ++ allVarsList es

-- True if name is in the list, False otherwise
checkName :: Name -> [Name] -> Bool
checkName name (x : xs) =
  if name == x
    then True
    else checkName name xs
checkName _ [] = False

-- get the new name for `p` if `p` taken
freeName :: Name -> [Name] -> Name
freeName name takenNames =
  if checkName name takenNames
    then freeName (name ++ "'") takenNames
    else name

-- change all occurences of old to new
subst :: (Name, Expr) -> Expr -> Expr
subst (old, new) (Var n)
  | n == old = new
  | otherwise = Var n
subst (old, new) (e1 :$ e2) = subst (old, new) e1 :$ subst (old, new) e2

-- change one param from params to not taken name
renameStep :: (Expr, [Name], [Name]) -> Name -> (Expr, [Name], [Name])
renameStep (body, forbidden, newParams) oldP =
  let newP = freeName oldP forbidden
      updatedForbidden = newP : forbidden
      updatedBody = subst (oldP, Var newP) body
   in (updatedBody, updatedForbidden, newP : newParams)

-- change all params to free names, reverse for keeping params order
renameDef :: Def -> [Name] -> Def
renameDef (Def dName params body) forbidden =
  let newForbidden = forbidden ++ params
      (finalBody, _, finalParams) = foldl renameStep (body, newForbidden, []) params
   in Def dName (reverse finalParams) finalBody

-- reduction utils

-- get as root and list of arguments
getAsList :: Expr -> [Expr] -> (Expr, [Expr])
getAsList (e1 :$ e2) args = getAsList e1 (e2 : args)
getAsList e args = (e, args)

-- get root and arguments together
buildApp :: Expr -> [Expr] -> Expr
buildApp expr [] = expr
buildApp expr (a : as) = buildApp (expr :$ a) as

-- substitues arguments for parameters
substList :: [Name] -> [Expr] -> Expr -> Expr
substList [] _ body = body
substList _ [] body = body
substList (p : ps) (a : as) body =
  let newBody = subst (p, a) body
   in substList ps as newBody

-- applies concrete definition
applyDef :: Def -> [Expr] -> Expr
applyDef (Def dName params body) args =
  let neededArgs = take (length params) args
      leftoverArgs = drop (length params) args

      forbidden = allVarsList neededArgs

      (Def _ safeParams safeBody) = renameDef (Def dName params body) forbidden

      reducedBody = substList safeParams neededArgs safeBody
   in buildApp reducedBody leftoverArgs

-- performs reduction on children when the root cannot be reduced
reduceChildren :: DefMap -> Expr -> Maybe Expr
reduceChildren dmap (left :$ right) =
  case rstep dmap left of
    Just newLeft -> Just (newLeft :$ right)
    Nothing -> case rstep dmap right of
      Just newRight -> Just (left :$ newRight)
      Nothing -> Nothing
reduceChildren _ (Var _) = Nothing

rstep :: DefMap -> Expr -> Maybe Expr
rstep dmap expr =
  let (rootExpr, args) = getAsList expr []
   in case rootExpr of
      Var name ->
        case Map.lookup name dmap of
          Just (Def dName params body) ->
            if length args >= length params
              then Just (applyDef (Def dName params body) args)
              else reduceChildren dmap expr
          Nothing -> reduceChildren dmap expr
      _ -> reduceChildren dmap expr -- chyba niemożliwe?

rpath :: DefMap -> Expr -> [Expr]
rpath dmap e = e : case rstep dmap e of 
  Nothing -> []
  Just nextE -> rpath dmap nextE

printPath :: DefMap -> Expr -> IO ()
printPath dmap e = putStr(unlines [show step | step <- (take 30 (rpath dmap e))])

-- program validation

-- check for duplicate definition names
checkDupNames :: [Def] -> [Name] -> Maybe String
checkDupNames [] _ = Nothing
checkDupNames (Def name _ _ : ds) seen =
  if checkName name seen
  then Just ("Error: combinator '" ++ name ++ "' is used several times.")
  else checkDupNames ds (name : seen)

-- checks for duplicates on the list
hasDuplicates :: [Name] -> [Name] -> Bool
hasDuplicates [] _ = False
hasDuplicates (x:xs) seen = 
  if checkName x seen 
    then True 
    else hasDuplicates xs (x : seen)

-- check for duplicate params in the definition
checkDupParams :: [Def] -> Maybe String
checkDupParams [] = Nothing
checkDupParams (Def name params _ : ds) =
  if hasDuplicates params []
  then Just ("Error: There are repeated arguments n combinator '" ++ name ++ "'.")
  else checkDupParams ds

-- check whether main defitinion is occurs and is correct
checkMainDef :: DefMap -> Maybe String
checkMainDef dmap =
  case Map.lookup "main" dmap of
    Nothing -> Just "Error: no `main` definition."
    Just (Def _ params _) -> 
      if length params == 0
        then Nothing
        else Just "Error: `main` should have 0 parameters."

-- check all conditions above
validateProgram :: [Def] -> DefMap -> Maybe String
validateProgram defs dmap =
  case checkDupNames defs [] of
    Just err -> Just err
    Nothing -> case checkDupParams defs of
      Just err -> Just err
      Nothing -> checkMainDef dmap
