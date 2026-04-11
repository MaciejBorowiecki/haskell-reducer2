module Main where

import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment

data Def = Def Name [Pat] Expr deriving Show
data Expr = Var Name | Expr :$ Expr deriving Show
type Pat = Name
type Name = String
newtype Prog = Prog {progDefs :: [Def]} deriving Show

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ["--help"] -> usage
    [] -> getContents >>= putStrLn
    [f] -> readFile f >>= putStrLn
    _ -> usage


usage :: IO ()
usage = do 
  putStrLn "pass"


fromHsString :: String -> Prog
fromHsString code = Prog (fromParseResult (parseModule code))

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult (ParseOk moduleTree) = fromHsModule moduleTree 
fromParseResult (ParseFailed loc msg) = error $ "syntax error while parsing" ++ msg

-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]
fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) = map fromHsDecl decls

-- HsFunBind (HsMatch SrcLoc HsName [HsPat] HsRhs [HsDecl])
fromHsDecl :: HsDecl -> Def
fromHsDecl (HsFunBind [HsMatch _ (HsIdent name) hsArgs (HsUnGuardedRhs hsExpr) _]) 
  = Def name (map fromHsArg hsArgs) (fromHsExpr hsExpr)
fromHsDecl _ = error "todo"

fromHsArg :: HsPat -> Pat
fromHsArg (HsPVar (HsIdent name)) = name
fromHsArg _ = error "todo"

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsVar (UnQual (HsIdent name))) = Var name
fromHsExpr _ = error "todo"
