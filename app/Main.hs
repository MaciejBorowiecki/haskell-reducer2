module Main where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment
import qualified Data.Map as Map

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
    [] -> getContents >>= putStrLn
    [f] -> do
      code <- readFile f
      let prog = fromHsString code
      putStrLn "--- Def list (prog) ---"
      putStr (show prog)
      putStrLn "\n--- DefMap  ---"
      putStrLn (show (buildDefMap prog))
    _ -> usage


usage :: IO ()
usage = do 
  putStrLn "pass"

-- parser utils

fromHsString :: String -> Prog
fromHsString code = Prog (fromParseResult (parseModule code))

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseOk moduleTree) = fromHsModule moduleTree 
fromParseResult (ParseFailed _ msg) = error $ "syntax error while parsing" ++ msg

-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]
fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) = map fromHsDecl decls

-- HsFunBind (HsMatch SrcLoc HsName [HsPat] HsRhs [HsDecl])
fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind [HsMatch _ (HsIdent name) hsArgs (HsUnGuardedRhs hsExpr) _]) 
  = Def name (map fromHsArg hsArgs) (fromHsExpr hsExpr)  
fromHsDecl (HsPatBind _ (HsPApp (UnQual (HsIdent name)) hsArgs) (HsUnGuardedRhs hsExpr) _)
  = Def name (map fromHsArg hsArgs) (fromHsExpr hsExpr)
fromHsDecl (HsPatBind _ (HsPVar (HsIdent name)) (HsUnGuardedRhs hsExpr) _)
  = Def name [] (fromHsExpr hsExpr)
fromHsDecl _ = error "fromHsDecl"

fromHsArg :: HsPat -> Pat
fromHsArg (HsPVar (HsIdent name)) = name
fromHsArg (HsPApp (UnQual (HsIdent name)) []) = name
fromHsArg (HsPParen inter) = fromHsArg inter
fromHsArg _ = error "fromHsArg"

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr (HsCon (UnQual (HsIdent name))) = Var name
fromHsExpr (HsApp e1 e2) = fromHsExpr e1 :$ fromHsExpr e2
fromHsExpr (HsParen e) = fromHsExpr e
fromHsExpr _ = error "fromHsExpr"

insertDefMapHelper :: Def -> DefMap -> DefMap
insertDefMapHelper (Def name args expr) dmap = Map.insert name (Def name args expr) dmap

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = foldr insertDefMapHelper Map.empty defs

-- name conflicts

-- list of all names in expression
allVars :: Expr -> [Name]
allVars (Var n) = [n]
allVars (e1 :$ e2) = allVars e1 ++ allVars e2

-- True if name is in the list, False otherwise
checkName :: Name -> [Name] -> Bool
checkName name (x:xs) = if name == x 
  then True
  else checkName name xs
checkName name [] = False

-- get the new name for `p` if `p` taken
freeName :: Name -> [Name] -> Name
freeName name takenNames = if checkName name takenNames 
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
