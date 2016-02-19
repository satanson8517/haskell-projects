-- POZNAMKY, TODO smazat
-- K projektu: Pavel
-- Zacit gramatikou, nemusi byt LL, parser umi rozpoznat A->BC, A->BD, vyzkousi oboje. Nemusi byt deterministicke.
-- Debugovani: udelat si vypisovaci prikazy
-- $ v kódu znamená, e v¹echno za ním se vyhodnotí napøed (nevyhodnocuje se klasicky zleva)
-- pred interpretaci zavolat funkci pro semantickou kontrolu, zde neni. Napr. definice dvou prom stejneho jmena. 
-- nacitani retezce do programu (V projektu vytvorit alias na string (definovat vlastni datovy typ) a pouzit misto String, prehlednejsi)
-- PREKLAD: ghc --make demo.hs

-- Interpret jazyka FLP-2014-Pascal
-- Projekt do predmetu FLP
-- Autori:
-- Radek Jelinek, xjelin22@stud.fit.vutbr.cz
-- Pavel Cacek, xcacek01@stud.fit.vutbr.cz
-- Pavel Kefurt, xkefur00@stud.fit.vutbr.cz
-- Jiri Drdla, xdrdla02@stud.fit.vutbr.cz



-- Pocatecni bod programu
module Main( main ) where

-- Import knihoven
import System.Environment( getArgs )                         -- knihovna pro parsovani argumentu
import System.IO                                             -- knihovna pro vstup a vystup

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language



-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--                         PARSOVANI
-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

-- Parsec - nastaveni
aelDef = emptyDef
  { commentStart   = "{"
  , commentEnd     = "}"
  , nestedComments = False
  , identStart     = letter <|> char '_' -- identifikator prvni pismeno
  , identLetter    = alphaNum <|> char '_'
  , opStart        = oneOf "=+*-d<>"  -- prvni znak operatoru
  , opLetter       = opStart aelDef
  , reservedOpNames= [ ":=", "+", "*", "-", "div", "=", "<>", "<", ">", "<=", ">=" ] -- operatory
  , reservedNames  = [ "writeln", "readln", "while", "do", "if", "then", "else", "begin", "end.", "end", "var", "integer", "double", "string", "function" ]  -- klicova slova
  , caseSensitive  = True
  }

-- Vytvoreni lexikalniho analyzatoru
lexer = P.makeTokenParser aelDef

-- Vytvoreni parseru pro elementarni casti programu
whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
parens    = P.parens lexer     -- parsuje ()
semi      = P.semi lexer
comma     = P.comma lexer
colon     = P.colon lexer
dot       = P.dot lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

-- Pocatecni neterminal gramatiky
aep = do          -- do: chic aby slo sekvencne
  whiteSpace      -- cekam nejaky pocet bilych znaku (nemusi zacinat znakem)
  ast <- program  -- dale cekam prikaz
  eof             -- vyhleda konec souboru
  return ast      -- musim neco vracetpomoci returnu
  <?> "aep"       -- <?> vypisovani chyb v monade, pokud neco selze, vyhodi chybu a vrati retezec (jako 'case else')
                  -- vsechny parsery parsuji dokud mohou

-- Interni reprezentace prikazu interpretovaneho programu, AST
data Command = Empty                                 -- Prazdny prikaz
  | SaveVar String VarType                           -- Ulozeni hodnoty do tabulky symbolu
  | ParamVar String VarType
  | DefFunc String  Command  VarType Command Command -- Definice funkce (jmeno, parametry, navratovy typ, lokalni promenne, telo)
  | DecFunc String  Command  VarType                 -- Deklarace funkce (jmeno, parametry, navratovy typ)
  | Assign String Expr                               -- Prirazeni do promenne
  | Print Expr                                       -- Vypis na standardni vystup
  | Scan String                                      -- Nacteni ze standardniho vstupu
  | Seq [ Command ]                                  -- Sekvence prikazu
  | If BoolExpr Command Command                      -- IF-THEN-ELSE
  | While BoolExpr Command                           -- WHILE cyklus
  deriving Show

program = do
    --TODO lehky problem muze byt vickrat var
    prom <- option (Empty)  vars
    dfc  <- many $ try (deffunc) <|> (decfunc)
    reserved "begin"
    seq <- sepBy cmd semi
    reserved "end."
    return $ Seq ([prom]++dfc++seq)
  <?> "program"

vartype = do
    reserved "integer"
    return $ VarInt
  <|> do
    reserved "double"
    return $ VarDouble
  <|> do
    reserved "string"
    return $ VarString
  <?> "vartype"

onevar = do
    i <- identifier
    colon
    v <- vartype
    return $ SaveVar i v
  <?> "onevar"

vars = do
      reserved "var"
      seq <- sepBy onevar comma
      semi
      return $ Seq seq
    <?> "vars" 

fvar = do
      i <- identifier
      colon
      v <- vartype
      return $ ParamVar i v
    <?> "fvar" 

decfunc = do
      reserved "function"
      name <- identifier
      seq <- parens $ sepBy fvar comma
      retval <- vartype
      semi
      return $ DecFunc name (Seq seq) retval
    <?> "decfunc" 

deffunc = do
      reserved "function"
      name <- identifier
      seq <- parens $ sepBy fvar comma
      retval <- vartype
      semi
      v <- option (Empty) vars
      try (do { 
            reserved "begin";
            cmds <- sepBy cmd semi;
            reserved "end";
            return $ DefFunc name (Seq seq) retval v (Seq cmds) }) 
           <|> 
          (do {
            i <- identifier;
            reserved ":=";
            e <- expr;
            if name == i then
              return $ DefFunc name (Seq seq) retval v (Assign i e) 
            else error "spany navrat z funkce"})
              
    <?> "deffunc" 

-- Parsovani prikazu programu
cmd = do                   -- Tisk na standardni vystup
    reserved "writeln"
    e <- parens expr       -- Parsovani vyrazu k tisku
    return $ Print e
  <|> do                   -- Nacteni do promenne
    reserved "readln"
    i <- parens identifier
    return $ Scan i
  <|> do                   -- Prizareni do promenne
    i <- identifier
    reservedOp ":="
    e <- expr
    return $ Assign i e
  <|> do                   -- IF-THEN-ELSE
    reserved "if"
    b <- boolExpr
    reserved "then"
    c1 <- cmd
    reserved "else"
    c2 <- cmd
    return $ If b c1 c2
  <|> do                   -- WHILE cyklus
    reserved "while"
    b <- boolExpr
    reserved "do"
    c <- cmd
    return $ While b c
  <|> do                   -- Sekvence prikazu
    reserved "begin"
    seq <- sepBy cmd semi
    reserved "end"
    return $ Seq seq
  <?> "command"


-- Parsovani vyrazu pomoci knihovni funkce
-- Parametry: aritmeticke operatory, asociativita
expr = buildExpressionParser operators term where
  operators = [                                            -- 1. param tabulka operatoru (seznam seznamù, podle priorit)
      [ op "*" Mult, op "div" Div ],                       -- nejvyssi priorita
      [ op "+" Add, op "-" Sub ]                           -- nizsi priorita
    ]
  op name fun =                                            -- 2. param je parser (term)
    Infix ( do { reservedOp name; return fun } ) AssocLeft -- bere operator a vraci datovy typ

-- Nejmensi cast, kterou chci v programu najit. Konstanta nebo identifikator.
term = do
    i <- integer
    return $ Const $ fromInteger i
  <|> do
    v <- identifier   -- identifikator
    try (do{ex <- parens $ sepBy expr comma;return $ Func v ex}) <|> (do{return $ Var v})
  <|> parens expr     -- zavorky
  <?> "term"          --

-- Parsovani booleovskych vyrazu
boolExpr = do
    e1 <- expr
    o <- relOp
    e2 <- expr
    return $ o e1 e2
  <?> "boolean expression"
  where
    relOp = ro' "=" Equal
      <|> ro' "<>" NotEqual
      <|> ro' "<" Less
      <|> ro' ">" Greater
      <|> ro' "<=" LessOrEqual
      <|> ro' ">=" GreaterOrEqual
      <?> "relational operator"
    ro' name fun = do
      reservedOp name
      return fun

	  
-- DATOVE STRUKTURY
	  
-- Datove typy
data VarType = VarInt | VarString | VarDouble deriving (Show, Eq)

-- Deklarace funkci
data FunctionDec = FunctionDec String [(String, VarType)] VarType deriving Show

-- Definice funkci
data FunctionDef = FunctionDef { name :: String,
                    countOfDeclaration :: Int,
                    params :: [(String, Expr, VarType)],
                    retType :: VarType,
                    locVars :: SymbolTable,
                    cmds :: [Command],
                    -- TODO dva spodni radky smazat
                    retExpr :: Expr,
                    retValue :: Expr } deriving Show

-- Vyrazy
data Expr = Const Int  -- celociselna konstanta
  | Var String      -- promenna identifikovana 
  | Add Expr Expr   -- soucet
  | Func String [ Expr ]
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving Show

-- Booleovske vyrazy
data BoolExpr = Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | Greater Expr Expr
  | LessOrEqual Expr Expr
  | GreaterOrEqual Expr Expr 
  deriving Show








-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--                        INTERPRETACE
-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

-- DEFINUJI TABULKU SYMBOLU
--TODO Int predelat na Expr
type SymbolTable = [(String, Int, VarType)]
type MainTable = ([SymbolTable], [FunctionDef]) 

--TODO nastavovat (Expr, VarType)
cre :: SymbolTable -> String -> VarType -> SymbolTable
--TODO tady nebude 0 ale "" protoze pracujeme nejenom s Intama
cre [] n VarString = [(n, 0, VarString)] 
cre [] n VarInt = [(n, 0, VarInt)] 
--TODO tady nebude 0 ale 0.0 protoze pracujeme nejenom s Intama
cre [] n VarDouble = [(n, 0, VarDouble)] 
cre (s@(na, _, _):ss) n t = if n == na then
                                      error "duplicitni promena"
                                    else s : cre ss n t

isin :: SymbolTable -> String -> Bool
isin [] _ = False
isin ((na, _, _):ss) n = if na == n then True
                          else isin ss n

set :: [SymbolTable] -> String -> Int -> VarType -> [SymbolTable]
set sts n v t = if isin (head sts) n then
                  [(smallset (head sts) n v t)]++(tail sts)
                else
                  if isin (last sts) n then
                    (init sts)++[(smallset (last sts) n v t)]
                  else
                    error "set: promena neexistuje"

--TODO nastavovat (Expr, VarType)
smallset :: SymbolTable -> String -> Int -> VarType -> SymbolTable
smallset [] _ _ _ = error "chyba" 
smallset (s@(na, va, ty):ss) n v t = if n == na && t == ty then
                                 (n, v, t):ss
                                  else s : smallset ss n v t

get :: [SymbolTable] -> String -> Int
get sts n = if isin (head sts) n then
                  smallget (head sts) n
                else
                  if isin (last sts) n then
                    smallget (last sts) n
                  else
                    error "set: promena neexistuje"


--TODO vracet (Expr, VarType)
smallget :: SymbolTable -> String -> Int   
smallget [] _ = error "chyba" 
smallget (s@(na, va, ty):ss) n =
  if n == na
    then va
    else smallget ss n

evaluate :: [SymbolTable] -> Expr -> Int
evaluate sts (Const i) = i
evaluate sts (Var v) = get sts v
evaluate sts (Add e1 e2) = (evaluate sts e1) + (evaluate sts e2)
evaluate sts (Sub e1 e2) = (evaluate sts e1) - (evaluate sts e2)
evaluate sts (Mult e1 e2) = (evaluate sts e1) * (evaluate sts e2)
evaluate sts (Div e1 e2) = (evaluate sts e1) `div` (evaluate sts e2)

decide :: [SymbolTable] -> BoolExpr -> Bool
decide sts (Equal a b) = evaluate sts a == evaluate sts b
decide sts (NotEqual a b) = evaluate sts a /= evaluate sts b
decide sts (Less a b) = evaluate sts a < evaluate sts b
decide sts (Greater a b) = evaluate sts a > evaluate sts b
decide sts (LessOrEqual a b) = evaluate sts a <= evaluate sts b
decide sts (GreaterOrEqual a b) = evaluate sts a >= evaluate sts b

interpret :: MainTable -> Command -> IO MainTable

-- Pokud prijde Empty, pouze vracim tabulku symbolu
-- Musim mit return kvuli zabaleni do monady
interpret mt (Empty) = return mt 

-- TODO operace init pri velkych seznamech je narocna ve velkych seznamech
interpret (sts, tf) (SaveVar n t) = return $ ([(cre (head sts) n t)]++(tail sts), tf)

-- fce scan
interpret (sts, tf) (Scan v) = do
  i <- readLn :: IO Int
--TODO VarInt to vzdy nebude
  return $ ((set sts v i VarInt), tf)

interpret mt@(sts, tf) (Print e) = do
  putStrLn $ show $ evaluate sts e
  return mt

-- interpretace sekvence prikazu.
interpret mt (Seq []) = return mt   -- Pevny bod, musim mit zarazku.
interpret mt (Seq (c:cs)) = do      
  mt' <- interpret mt c
  interpret mt' $ Seq cs            -- return se sem dostane z jineho prikazu

interpret mt@(sts, tf) (If cond c1 c2) = do
  if decide sts cond
    then interpret mt c1
    else interpret mt c2

interpret mt@(sts, tf) w@(While cond c) = do
  if decide sts cond
    then do
      mt' <- interpret mt c
      interpret mt' w
    else return mt

interpret mt@(sts, tf) (Assign v e) = do
  return $ ((set sts v (evaluate sts e) VarInt), tf)

--  | DefFunc String  Command  VarType Command Command
--  | DecFunc String  Command  VarType
interpret mt@(sts, tf) (DecFunc n (Seq p) t) = return mt

-- Vyuzivam funkci evaluate berouci vyraz a vracejici cislo
-- Nejdriv udelam ($ evaluate ts e), pak ($ set ts v...) a potom return  
--TODO VarInt to vzdy nebude

-- I/O akce, potrebuji monadu 'do'. Konvertuji na string, vracim tabulku symbolu jak jsem dostal, nic se nemìní.

-- FIXME toto je test
--interpret ts (SaveVar s v) = do
 -- putStr $ show $ s
  --putStrLn $ show $ v 
 -- return ts
--interpret ts (DecFunc n s v) = do
 -- putStr $ show $ n
 -- putStr $ show $ s
 -- putStrLn $ show $ v
 -- return ts
--interpret ts (DefFunc n s v t r) = do
 {- putStr $ show $ n
  putStr $ show $ s
  putStr $ show $ v 
  putStrLn $ show $ t
  putStrLn $ show $ r 
  return ts
-}
  
  
-- vlastni fce parsecu bere 3 parametry, vraci chybiu nebo vysledek (jako maybe)
parseAep input file =
  case parse aep file input of
    Left e -> error $ show e      -- left=konstruktor pokud neuspelo
    Right ast -> ast    -- pokud uspelo, vraci abstr synt strom
    
-- pr fce parsecu: parse letter "" "a"
-- vytvoreni parseru cisel NEBO pismen:
-- parse (letter <|> digit) "" "?"




-- POCATECNI FUNKCE 
    
main = do
  args <- getArgs
  if length args /= 1
    then error "Specify one input file."
    else do
      let fileName = args!!0  -- jestli je to seznam, beru nulty prvek; LET dovoluje definovat vlastni, zde takovou inline, funkci.
      input <- readFile fileName
      let ast = parseAep input fileName   -- parseAep: bere obsah souboru a vola fci parse(definovana v parsecu)
      -- prvni tabulka je seznamem tabulek promnenych a ma prvni seznam globalnich promnenych ktery je prazny
      interpret ([[]], []) ast                    -- volani interpretu, ma za vstup abstrakt synt strom

--end demo.hs
