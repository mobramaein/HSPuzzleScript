{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Error
import Data.IORef

-- IO Types --
type Env = IORef [(String, IORef PzlVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT PzlError IO

-- IO Helpers --

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError PzlVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> PzlVal -> IOThrowsError PzlVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> PzlVal -> IOThrowsError PzlVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, PzlVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- Pzl Datatypes --


data KeywordVal = Keyword String
                | Direction String
                | LogicWord String
instance Show KeywordVal where show = showKeyword

showKeyword :: KeywordVal -> String
showKeyword (Keyword w) = w
showKeyword (Direction d) = d
showKeyword (LogicWord l) = l

data PatternVal = Pattern KeywordVal PzlVal KeywordVal PzlVal
-- instance Show PatternVal where show = showPattern

-- showPattern :: PatternVal -> String

data PzlVal = GameObject String -- TODO: A lot
            | Rule PatternVal PatternVal
            | WinCondition KeywordVal PzlVal KeywordVal PzlVal
            | Level [String]
            | Func { params :: [String], vararg :: (Maybe String),
                     body :: [PzlVal], closure :: Env }
            | IOFunc ([PzlVal] -> IOThrowsError PzlVal)
            | Port Handle
            | Atom String
            | List [PzlVal]
            | DottedList [PzlVal] PzlVal
            | Number Integer
            | String String
            | Bool Bool
            | PrimitiveFunc ([PzlVal] -> ThrowsError PzlVal)
instance Show PzlVal where show = showVal

showVal :: PzlVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive"
showVal (GameObject name) = name
-- showVal (Rule logic preC postC) = logic ++ preC ++ postC
-- showVal (WinCondition logic1 obj1 logic2 obj2) = logic1 ++ obj1 ++ logic2 ++ obj2
showVal (PrimitiveFunc _ ) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [PzlVal] -> String
unwordsList = unwords . map showVal

data PzlError = NumArgs Integer [PzlVal]
                | TypeMismatch String PzlVal
                | Parser ParseError
                | BadSpecialForm String PzlVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
instance Show PzlError where show = showError

instance Error PzlError where
        noMsg = Default "An error has occurred"
        strMsg = Default

type ThrowsError = Either PzlError

showError :: PzlError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invlaid type: expected: " ++ expected
                                        ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data Unpacker = forall a. Eq a => AnyUnpacker (PzlVal -> ThrowsError a)

-- IO Helpers --

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- REPL --

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Pzl>>> ") . evalAndPrint

-- Parsing --

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<==>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser PzlVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser PzlVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser PzlVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser PzlVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser PzlVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser PzlVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Stupid Version --
parseRule :: Parser PzlVal
parseRule = do
    char '['
    keyW1 <- many (noneOf " ")
    char ' '
    let dir1 = Direction keyW1
    char ' '
    obj1 <- many (noneOf " ")
    let obVal1 = GameObject obj1
    char '|'
    keyW2 <- many (noneOf " ")
    char ' '
    let dir2 = Direction keyW2
    char ' '
    obj2 <- many (noneOf " ")
    let obVal2 = GameObject obj2
    char ']'
    let preC = Pattern dir1 obVal1 dir2 obVal2

    let rule = Rule preC preC
    return rule


parseExpr :: Parser PzlVal
parseExpr = parseAtom
        <|> parseRule
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
-- File IO --
ioPrimitives :: [(String, [PzlVal] -> IOThrowsError PzlVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [PzlVal] -> IOThrowsError PzlVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [PzlVal] -> IOThrowsError PzlVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [PzlVal] -> IOThrowsError PzlVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [PzlVal] -> IOThrowsError PzlVal
readProc []          = readProc [Port stdin]
-- liftIO and liftThrows convert readExpr and hGetLine to the IOThrowsError monad
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [PzlVal] -> IOThrowsError PzlVal
writeProc [obj]           = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [PzlVal] -> IOThrowsError PzlVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [PzlVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- Loads, and wraps result in a list
readAll :: [PzlVal] -> IOThrowsError PzlVal
readAll [String filename] = liftM  List $ load filename


-- Primitives --

primitives :: [(String, [PzlVal] -> ThrowsError PzlVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                             ++ map (makeFunc PrimitiveFunc) primitives)
      where makeFunc constructor (var, func) = (var, constructor func)

numericBinop :: (Integer -> Integer -> Integer) -> [PzlVal] -> ThrowsError PzlVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (PzlVal -> ThrowsError a) -> (a -> a -> Bool) -> [PzlVal] -> ThrowsError PzlVal
boolBinop unpacker op args = if length args /= 2
                            then throwError $ NumArgs 2 args
                            else do left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $  left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: PzlVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: PzlVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: PzlVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch " number" notNum

unpackEquals :: PzlVal -> PzlVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
            do unpacked1 <- unpacker arg1
               unpacked2 <- unpacker arg2
               return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

-- List Primitives --

car :: [PzlVal] -> ThrowsError PzlVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [PzlVal] -> ThrowsError PzlVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [PzlVal] -> ThrowsError PzlVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [PzlVal] -> ThrowsError PzlVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val

eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [PzlVal] -> ThrowsError PzlVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Eval/Apply --

eval :: Env -> PzlVal -> IOThrowsError PzlVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             otherwise  -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body

eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)

eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized Special Form" badForm

apply :: PzlVal -> [PzlVal] -> IOThrowsError PzlVal
apply (PrimitiveFunc func) args = liftThrows $ func args
-- for user defined funcs..wtF?
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env


makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

-- Main --

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "pzl" input of
   Left err -> throwError $ Parser err
   Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
