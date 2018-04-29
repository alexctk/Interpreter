


import Data.Char
import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)
import Data.Maybe

data SExpression = NumAtom Int | SymAtom [Char] | List [SExpression] deriving (Show)

type Identifier = [Char]

data SmLispExpr = SExpr SExpression |
                  Variable Identifier |
                  FnCall Identifier [SmLispExpr] |
                  CondExpr [CondClause] |
                  LetExpr [LocalDef] SmLispExpr deriving (Show)

type CondClause = (SmLispExpr, SmLispExpr)
type LocalDef = (Identifier, SmLispExpr)

data Definition = ConstantDef Identifier SmLispExpr |
                  FunctionDef Identifier [Identifier] SmLispExpr deriving (Show)

type SmLispProgram = [Definition]

data Token = Comment [Char] |
             NumToken Int |
             AlphaNumToken [Char] |
             SpecialToken [Char] |
             Lparen | Rparen | Lbrak | Rbrak | Lbrace | Rbrace | Equal | Semicolon | Arrow | Quote | Colon deriving (Show)


--
-- Tokenizer
--


-- separate into potential tokens
-- i.e. numbers, words, special tokens should be split
split_file :: [Char] -> [[Char]]
split_file [] = []
split_file (c:rest)
  -- ignore whitespace
  | isSpace c = split_file rest
  -- ignore newlines
  | c == '\n' = split_file rest
  -- get the longest prefix of alphas, apply split to the rest
  | isAlpha c = (fst (span isAlpha (c:rest))) : (split_file (snd (span isAlpha (c:rest))))
  -- get the longest prefix of digits
  | isDigit c = (fst (span isDigit (c:rest))) : (split_file (snd (span isDigit (c:rest))))
  | otherwise = [c] : (split_file rest)

--want to combine symbols like dashes in function names
--will tokenize first, then apply some simplification if,
--for example, there are multiple AlphaNumTokens in a row

tokenize :: [[Char]] -> [Token]
tokenize [] = []
tokenize (['-']:['-']:['>']:more) = (Arrow) : (tokenize more)
--tokenize ((';':';':';':[]):more) = 
tokenize ((c:cs):more)
  | isAlpha c = (AlphaNumToken (c:cs)) : (tokenize more)
  | isDigit c = (NumToken (read (c:cs) :: Int)) : (tokenize more)
  | c == '('  = (Lparen) : (tokenize more)
  | c == ')'  = (Rparen) : (tokenize more)
  | c == '['  = (Lbrak) : (tokenize more)
  | c == ']'  = (Rbrak) : (tokenize more)
  | c == '{'  = (Lbrace) : (tokenize more)
  | c == '}'  = (Rbrace) : (tokenize more)
  | c == ';'  = (Semicolon) : (tokenize more)
  | c == '"'  = (Quote) : (tokenize more)
  | c == ':'  = (Colon) : (tokenize more)
  | c == '='  = (Equal) : (tokenize more)
  | otherwise = (SpecialToken (c:cs)) : (tokenize more)

--"bandaid" function to get rid of certain undesirable token patterns
--given more time, i'd integrate these into the tokenizer or parser
simplify :: [Token] -> [Token]
--when a '-' is part of an identifier
simplify ((AlphaNumToken c):(SpecialToken "-"):(AlphaNumToken d):more) = simplify ((AlphaNumToken (c++"-"++d)):more)
--make an Arrow
simplify ((AlphaNumToken "-"):(AlphaNumToken "-"):(AlphaNumToken ">"):more) = (simplify (Arrow:more))
--when a number is part of an identifier
simplify ((AlphaNumToken c):(NumToken n):more) = (AlphaNumToken (c++(show n))):(simplify more)
--combine sequences of ANTs
--simplify ((AlphaNumToken x):(AlphaNumToken y):more) = simplify $ AlphaNumToken (x++y) : (simplify more)
--to get rid of the double dashes in error2 call
simplify ((AlphaNumToken x):(SpecialToken "-"):(SpecialToken "-"):more) = simplify $ (AlphaNumToken (x++"-"++"-")) : (simplify more)
--if we can't do anything, simplify the rest
simplify (x:more) = x:(simplify more)
simplify (x:[]) = [x]
simplify x = x


--
-- Parser
--


--SExpression ::= <atom> | <list>
parse_s_expression :: [Token] -> (Maybe SExpression, [Token])
parse_s_expression s =
  case parse_list(s) of
    (Just e, more) -> (Just (e), more)
    _ -> case parse_atom(s) of
          (Just e, more) -> (Just e, more)
          _ -> (Nothing, [])

--Atom ::= <num-atom> | <sym-atom>
parse_atom :: [Token] -> (Maybe SExpression, [Token])
parse_atom s =
  case parse_n_atom(s) of
    (Just e, more) -> (Just e, more)
    _ -> case parse_s_atom(s) of
           (Just e, more) -> (Just e, more)
           _ -> (Nothing, [])
parse_atom s = (Nothing, [])

--Num-Atom ::= -?[0-9]+
parse_n_atom :: [Token] -> (Maybe SExpression, [Token])
parse_n_atom ((SpecialToken ['-']):(NumToken c):more) = (Just (NumAtom (-c)), more)
parse_n_atom ((NumToken c):more) = (Just (NumAtom c), more)
parse_n_atom s = (Nothing, [])

--Sym-Atom ::= [A-Za-z](-?[A-Za-z0-9])*
parse_s_atom :: [Token] -> (Maybe SExpression, [Token])
parse_s_atom ((AlphaNumToken c):more) = (Just (SymAtom c), more)
parse_s_atom ((SpecialToken c):more) = (Just (SymAtom c), more)
parse_s_atom s = (Nothing, [])

--List ::= '(' {<S-expression>} ')'
parse_list :: [Token] -> (Maybe SExpression, [Token])
parse_list (Lparen:Rparen:more) = (Just (List []), more)
parse_list (Lparen:more) =
  case parse_s_expression(more) of
    (Just e, Rparen:more) -> (Just (List [e]), more)
    --extend if we don't see a closing paren
    (Just e, more) -> extend_exp (List [e]) more
    _ -> (Nothing, [])
parse_list s = (Nothing, [])             


extend_exp :: SExpression -> [Token] -> (Maybe SExpression, [Token])
extend_exp s1 [] = (Just s1, [])
extend_exp (List s1) more =
  case parse_s_expression(more) of
    -- base case: we see closing parenthesis
    (Just s2, Rparen:yet_more) -> (Just (List (s2:s1)), yet_more)
    -- look for more SExpressions
    (Just s2, yet_more) -> extend_exp (List (s2:s1)) yet_more
    _ -> (Nothing, [])
    
--Expression ::= <value> | <variable> | <function-call> | <conditional-expression> | <let-expression>
parse_sm_lisp_expr :: [Token] -> (Maybe SmLispExpr, [Token])
-- The grammar has no symbols which differentiate the cases (unless we expand them)
-- so check cases sequentially?
-- ie. if it's not a value, then it could be a variable, if it's not a variable
-- it could be a function call etc.
-- seems excessive, but I want to be consistent with the grammar.
-- Eg. I could check for a '[' for a conditional expression, but I want that
-- to be done in the parsing function for conditional expressions
-- since <conditional-expression> contains the '['

--put parseFCall before parseVar since they both start with identifiers
--and FCall contains the more complex pattern
parse_sm_lisp_expr s =
  case parse_val(s) of
    (Just e, more) -> (Just e, more)
    _ -> case parse_f_call(s) of
          (Just f, more) -> (Just f, more)
          _ -> case parse_var(s) of
                (Just v, more) -> (Just v, more)
                _ -> case parse_c_exp(s) of
                      (Just c, more) -> (Just c, more)
                      _ -> case parse_l_exp(s) of
                            (Just l, more) -> (Just l, more)
                            _ -> (Nothing, [])

--Value ::= <num-atom> | '"' <sym-atom> '"' | <list> 
parse_val :: [Token] -> (Maybe SmLispExpr, [Token])
parse_val (Quote:more) =
  case parse_s_expression(more) of
    (Just e, Quote:more) -> (Just (SExpr e), more)
    _ -> (Nothing, [])
parse_val ((NumToken n):stuff) =
  case parse_s_expression((NumToken n):stuff) of
    (Just (NumAtom x), more) -> (Just (SExpr (NumAtom x)), more)
    _ -> (Nothing, [])
--list case
parse_val (Lparen:stuff) =
  case parse_s_expression(Lparen:stuff) of
    (Just e, more) -> (Just (SExpr e), more)
    _ -> (Nothing, [])
parse_val s = (Nothing, [])    

--Variable ::= <identifier>
parse_var :: [Token] -> (Maybe SmLispExpr, [Token])
parse_var s =
  case parse_id(s) of
    (Just e, more) -> (Just (Variable e), more)
    _ -> (Nothing, [])

--Identifier ::= [A-Za-z](-?[A-Za-z0-9])*
--same as an AlphaNumToken
parse_id :: [Token] -> (Maybe Identifier, [Token])
parse_id ((AlphaNumToken c):more) = (Just c, more)
parse_id s = (Nothing, [])


--Function-Call ::= <function-identifier> '[' <expression> {';' <expression>} ']';
parse_f_call :: [Token] -> (Maybe SmLispExpr, [Token])
parse_f_call ((AlphaNumToken c):Lbrak:more) =
  case parse_sm_lisp_expr(more) of
    (Just e, yet_more) -> extend_f_call c [e] yet_more
    _ -> (Nothing, [])
parse_f_call s = (Nothing, [])
        
extend_f_call :: Identifier -> [SmLispExpr] -> [Token] -> (Maybe SmLispExpr, [Token])
--either we see another semicolon or an Rbrak
--we reverse the final list due to how the list was formed
--otherwise, for example, can end up with problems when evaluating
--lists of conditions
extend_f_call f e (Rbrak:more) = (Just (FnCall f (reverse e)), more)
--keep extending
extend_f_call f e1 (Semicolon:more) =
  case parse_sm_lisp_expr(more) of
    (Just e2, yet_more) -> extend_f_call f (e2:e1) yet_more
    _ -> (Nothing, [])
extend_f_call f e1 s = (Nothing, [])
    
--Function-Identifier ::= <identifier>
parse_fid :: [Token] -> (Maybe Identifier, [Token])
parse_fid s = parse_id s

--Conditional-Expression ::= '[' <clause> {';' <clause>} ']'
--CondExpr [CondClause]
parse_c_exp :: [Token] -> (Maybe SmLispExpr, [Token])
parse_c_exp (Lbrak:more) =
  -- look for an inital clause and extend
  case parse_clause(more) of
    (Just c1, Semicolon:yet_more) -> extend_clause [c1] yet_more
    _ -> (Nothing, [])
parse_c_exp s = (Nothing, [])

extend_clause :: [CondClause] -> [Token] -> (Maybe SmLispExpr, [Token])
-- if we see an Rbrak it's over
extend_clause c1 (Rbrak:more) = (Just (CondExpr (reverse c1)), more)
-- if we see a Semicolon, extend
-- but parse_clause already deals with the semicolon?
-- i will keep this anyway just in case
extend_clause c1 (Semicolon:more) =
  case parse_clause(more) of
    (Just c2, yet_more) -> extend_clause (c2:c1) yet_more
    _ -> (Nothing, [])
extend_clause c1 more =
  case parse_clause(more) of
    (Just c2, yet_more) -> extend_clause (c2:c1) yet_more
    _ -> (Nothing, [])
extend_clause c s = (Nothing, [])    

--Clause ::= <expression> '-->' <expression>
parse_clause :: [Token] -> (Maybe CondClause, [Token])
parse_clause s =
  case parse_sm_lisp_expr(s) of
    (Just e1, Arrow:more) ->
      case parse_sm_lisp_expr(more) of
        (Just e2, yet_more) -> (Just (e1, e2), yet_more)
        _ -> (Nothing, [])
    _ -> (Nothing, [])

--Let-Expression ::= '{' <local-definition> {';' <local-definition>} ':' <expression> '}'
--LetExpr [LocalDef] SmLispExpr
parse_l_exp :: [Token] -> (Maybe SmLispExpr, [Token])
--parse_l_exp (Lbrace:more) =
--  case parse_l_def(more) of
--    (Just l1, Semicolon:yet_more) -> extend_l_exp [l1] yet_more
--    _ -> (Nothing, [])
--parse_l_exp s = (Nothing, [])
parse_l_exp (Lbrace:more) =
  case parse_l_def(more) of
    (Just l1, Colon:yet_more) ->
      case parse_sm_lisp_expr(yet_more) of
        (Just expr, Rbrace:even_more) -> (Just (LetExpr [l1] expr), even_more)
        _ -> (Nothing, [])
    (Just l1, Semicolon:yet_more) -> extend_l_exp [l1] yet_more
    _ -> (Nothing, [])
parse_l_exp s = (Nothing, [])    


extend_l_exp :: [LocalDef] -> [Token] -> (Maybe SmLispExpr, [Token])    
--terminate if we see a colon, then look for an expression and an Rbrace
--extend_l_exp l (Rbrace:more) =
--  case parse_sm_lisp_expr(more) of
--    (Just e, yet_more) -> (Just (LetExpr l e), yet_more)
--    _ -> (Nothing, [])
--extend_l_exp l (Semicolon:more) =
--  case parse_l_def(more) of
--    (Just l1, yet_more) -> extend_l_exp (l1:l) yet_more
--    _ -> (Nothing, [])
--extend_l_exp l s = (Nothing, [])

--termination symbol: Colon
extend_l_exp ldefs (Colon:more) =
  case parse_sm_lisp_expr(more) of
    (Just expr, Rbrace:yet_more) -> (Just (LetExpr (reverse ldefs) expr), yet_more)
    _ -> (Nothing, [])
extend_l_exp ldefs (Semicolon:more) =
  case parse_l_def(more) of
    (Just l, yet_more) -> extend_l_exp (l:ldefs) yet_more
    _ -> (Nothing, [])
extend_l_exp ldefs more =
  case parse_l_def(more) of
    (Just l, yet_more) -> extend_l_exp (l:ldefs) yet_more
    _ -> (Nothing, [])
extend_l_exp l s = (Nothing, [])


--Local-Definition ::= <identifier> '=' <expression>
parse_l_def :: [Token] -> (Maybe LocalDef, [Token])
parse_l_def s =
  case parse_id(s) of
    (Just e1, Equal:more) ->
      case parse_sm_lisp_expr(more) of
        (Just e2, even_more) -> (Just (e1, e2), even_more)
        _ -> (Nothing, [])
    _ -> (Nothing, [])
      
--Program ::= {<definition>}
parse_sm_lisp_program :: [Token] -> (Maybe SmLispProgram, [Token])
parse_sm_lisp_program s =
  case parse_defs(s) of
    -- look for more definitions
    (Just d, more) -> extend_defs [d] more
    _ -> (Nothing, [])


extend_defs :: [Definition] -> [Token] -> (Maybe [Definition], [Token])
extend_defs d1 [] = (Just (reverse d1), [])
extend_defs d1 more =
  case parse_defs(more) of
    (Just d2, yet_more) -> extend_defs (d2:d1) yet_more
    _ -> (Nothing, [])
    

--Definition ::= <constant-definition> | <function-definition>
parse_defs :: [Token] -> (Maybe Definition, [Token])
parse_defs s =
  case parse_c_def(s) of
    (Just d, more) -> (Just d, more)
    _ -> case parse_f_def(s) of
           (Just f, more) -> (Just f, more)
           _ -> (Nothing, [])

--Constant-Definition ::= [<comment>] <identifier> '=' <expression>
parse_c_def :: [Token] -> (Maybe Definition, [Token])
parse_c_def (Semicolon:Semicolon:Semicolon:more) = parse_c_def more
parse_c_def s =
  case parse_id(s) of
    (Just i, Equal:more) ->
      case parse_sm_lisp_expr(more) of
        (Just e, yet_more) -> (Just (ConstantDef i e), yet_more)
        _ -> (Nothing, [])
    _ -> (Nothing, [])
--ignore anything that isn't a recognizable pattern
-- those are likely to be comments
--parse_c_def ((AlphaNumToken c):more) = parse_c_def more   
parse_c_def (c:more) = parse_c_def more

--Function-Definition ::= [<comment>] <function-identifier> '[' <paramenter> {';' <parameter>} ']' '=' <expression>
--FunctionDef Identifier [Identifier] SmLispExpr
parse_f_def :: [Token] -> (Maybe Definition, [Token])
--identifing comments: three semicolons followed by (Optional) AlphaNumToken(s) and an Endline
--or we could look for the correct pattern of AlphaNumToken:Lbrak:more
--if we see three semicolons, skip the following AlphaNumTokens until we see correct pattern
parse_f_def (Semicolon:Semicolon:Semicolon:more) = parse_f_def more
-- c is the function identifier
-- then we look for one parameter
parse_f_def ((AlphaNumToken c):Lbrak:more) =
  case parse_param(more) of
    -- extend
    (Just p1, yet_more) -> extend_f_def c [p1] yet_more
    _ -> (Nothing, [])
--parse_f_def ((AlphaNumToken c):more) = parse_f_def more
--ignore comments
parse_f_def (c:more) = parse_f_def more
parse_f_def s = (Nothing, []) 

extend_f_def :: Identifier -> [Identifier] -> [Token] -> (Maybe Definition, [Token])
-- we see Rbrak Equal, look for an expression
extend_f_def i0 i1 (Rbrak:Equal:more) =
  case parse_sm_lisp_expr(more) of
    (Just e, yet_more) -> (Just (FunctionDef i0 (reverse i1) e), yet_more)
    _ -> (Nothing, [])
extend_f_def i0 i1 (Semicolon:more) =
  case parse_param(more) of
    (Just i2, yet_more) -> extend_f_def i0 (i2:i1) yet_more
    _ -> (Nothing, [])
extend_f_def i0 i1 s = (Nothing, [])    

--Parameter ::= <identifier>
parse_param :: [Token] -> (Maybe Identifier, [Token])
parse_param s = parse_id s


--Comment ::= (^;;;.*$)+
--Don't need to parse this

--main parsing function
--consumes all input
--splits file, tokenizes, then parses
--might return a small lisp program
parse_main :: [Char] -> Maybe SmLispProgram
parse_main fileString =
  case parse_sm_lisp_program(simplify (tokenize (split_file fileString))) of
    (Just p, []) -> Just p
    _ -> Nothing 



--
-- Primitive functions
--

sl_symbolp :: SExpression -> Maybe SExpression
sl_symbolp (SymAtom c) = Just (SymAtom "T")
sl_symbolp s           = Just (SymAtom "F")

sl_numberp :: SExpression -> Maybe SExpression
sl_numberp (NumAtom n) = Just (SymAtom "T") 
sl_numberp s           = Just (SymAtom "F")

sl_listp :: SExpression -> Maybe SExpression
sl_listp (List l) = Just (SymAtom "T")
sl_listp s        = Just (SymAtom "F")

sl_endp :: SExpression -> Maybe SExpression
sl_endp (List []) = Just (SymAtom "T")
sl_endp s         = Just (SymAtom "F")

sl_first :: SExpression -> Maybe SExpression
sl_first (List (c:cs)) = Just c
sl_first s             = Nothing

sl_rest :: SExpression -> Maybe SExpression
sl_rest (List (c:cs)) = Just (List cs)
sl_rest s             = Nothing

sl_cons :: SExpression -> SExpression -> Maybe SExpression
sl_cons x (List y) = Just (List (x:y))
sl_cons x y        = Nothing

sl_eq :: SExpression -> SExpression -> Maybe SExpression
sl_eq (SymAtom x) (SymAtom y)
  | x == y    = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
sl_eq x y = Nothing

sl_plus :: SExpression -> SExpression -> Maybe SExpression
sl_plus (NumAtom x) (NumAtom y) = Just (NumAtom (x+y))
sl_plus x y                     = Nothing

sl_minus :: SExpression -> SExpression -> Maybe SExpression
sl_minus (NumAtom x) (NumAtom y) = Just (NumAtom (x-y))
sl_minus x y                     = Nothing

sl_times :: SExpression -> SExpression -> Maybe SExpression
sl_times (NumAtom x) (NumAtom y) = Just (NumAtom (x*y))
sl_times x y                     = Nothing

sl_divide :: SExpression -> SExpression -> Maybe SExpression
sl_divide (NumAtom x) (NumAtom y)
  | y == 0 = Nothing
  | otherwise = Just (NumAtom ((signum x)*(signum y)*(abs (div x y))))
sl_divide x y = Nothing

sl_rem :: SExpression -> SExpression -> Maybe SExpression
sl_rem (NumAtom x) (NumAtom y)
  | y == 0 = Nothing
  | otherwise = (sl_minus (NumAtom x) (fromJust (sl_divide (NumAtom x) (NumAtom y))))
sl_rem x y = Nothing

sl_eqp :: SExpression -> SExpression -> Maybe SExpression
sl_eqp (NumAtom x) (NumAtom y)
  | x == y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
sl_eqp x y = Nothing

sl_lessp :: SExpression -> SExpression -> Maybe SExpression
sl_lessp (NumAtom x) (NumAtom y)
  | x < y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
sl_lessp x y = Nothing

sl_greaterp :: SExpression -> SExpression -> Maybe SExpression
sl_greaterp (NumAtom x) (NumAtom y)
  | x > y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
sl_greaterp x y = Nothing

sl_sym_lessp :: SExpression -> SExpression -> Maybe SExpression
sl_sym_lessp (SymAtom x) (SymAtom y)
  | x < y = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
sl_sym_lessp x y = Nothing

sl_explode :: SExpression -> Maybe SExpression
sl_explode (SymAtom (x:[])) = Just (List [(SymAtom [x])])
sl_explode (SymAtom (x:xs)) = sl_cons (SymAtom [x]) (fromJust (sl_explode (SymAtom xs)))
sl_explode s              = Nothing

sl_implode :: SExpression -> Maybe SExpression 
sl_implode (List ((List x):xs)) = Nothing
sl_implode (SymAtom (x:[])) = Just (SymAtom [x])
sl_implode (NumAtom x) = Just (SymAtom (show x))
sl_implode (List ((SymAtom x):more)) =
  case sl_implode(List more) of
    (Just (SymAtom y)) -> Just (SymAtom (x++y))
    _ -> Nothing
sl_implode s = Nothing    


--
-- Accessors and checkers
--
-- most are now unused, and were written
-- to aid in understanding the logic of the
-- small lisp interpreter in small lisp


fn_def_p (FunctionDef i iList e) = True
fn_def_p x = False

const_def_p (ConstantDef i e) = True
const_def_p x = False


local_frame :: Env -> AList
local_frame (l, g) = l

global_frame :: Env -> AList
global_frame (l, g) = g


numeric_val e = e
symbolic_val e = e
list_val e = e
callee (FnCall i s) = i
args (FnCall i s) = s
--clauses[cond-expr] = rest[cond-expr]
--make-cond-expr[clauses] = cons["cond";clauses]
clauses c = c
--final-expr[let-expr] = third[let-expr]
--make-let-expr[defs; expr] = list3["let"; defs; expr]
final_expr (LetExpr defs expr) = expr
--local-defs[let-expr] = second[let-expr]
local_defs (LetExpr defs expr) = defs

--
-- Environments
--

--environment, (local-frame, global-frame)
--in small lisp it is a list with two elements
type Env = (AList, AList)
-- association list (Name, value)
type AList = [([Char], SExpression)]

type FEnv = [([Char], Definition)]

         
-- environment, name, value
-- LocalDef (identifier, SmLispExpr)
extend_env :: Env -> [Char] -> SExpression -> Env
extend_env (loc, glob) name val = ( ((name, val):loc), glob)

extend_local_env :: FEnv -> Env -> SmLispExpr -> Maybe Env
extend_local_env fnenv valenv (LetExpr ((name, val):[]) sexpr) =
  case (sl_eval val fnenv valenv) of
    Just e -> Just (extend_env valenv name e)
    _ -> Nothing
extend_local_env fnenv valenv (LetExpr ((name, val):ldefs) sexpr) =
  case (sl_eval val fnenv valenv) of
    Just e ->
      case (extend_local_env fnenv valenv (LetExpr ldefs sexpr)) of
        Just env -> Just (extend_env env name e)
        _ -> Nothing
    _ -> Nothing


make_val_env :: AList -> AList -> Env
make_val_env lFrame gFrame = (lFrame, gFrame)

make_binding :: [Char] -> SExpression -> ([Char], SExpression)
make_binding sym val = (sym, val)

mark_global_frame :: Env -> Env
mark_global_frame e = make_val_env [] (local_frame e)

reset_to_global_frame :: Env -> Env
reset_to_global_frame e = make_val_env [] (global_frame e)

add_associations :: Env -> [Identifier] -> [SExpression] -> Env
add_associations e (p:params) (a:args)
  --single parameter
  | params == [] = extend_env e p a
  | otherwise    = add_associations (extend_env e p a) params args


extend_fn_env :: FEnv -> [Char] -> Definition -> FEnv
extend_fn_env [] name val = [(name, val)]
extend_fn_env env name val = (name, val):env


setup_envs_then_eval :: FEnv -> Env -> [Definition] -> SmLispExpr -> Maybe SExpression
setup_envs_then_eval fnenv valenv [] expr = sl_eval expr fnenv (mark_global_frame valenv)
setup_envs_then_eval fnenv valenv ((FunctionDef name args e):defs) expr =
      setup_envs_then_eval (extend_fn_env fnenv name (FunctionDef name args e)) valenv defs expr
setup_envs_then_eval fnenv valenv ((ConstantDef name e):defs) expr =
  case (sl_eval e fnenv valenv) of
    Just ex -> Just ex
    _ -> Nothing

sl_apply_fn_env :: FEnv -> [Char] -> Maybe Definition
sl_apply_fn_env fnenv name = lookup name fnenv


sl_apply_env :: Env -> [Char] -> Maybe SExpression
sl_apply_env (loc, glob) name =
  case (lookup name loc) of
    Just val -> Just val
    _ -> case (lookup name glob) of
          Just val -> Just val
          _ -> Nothing


interpret :: [Definition] -> SmLispExpr -> Maybe SExpression            
interpret defs expr =
  setup_envs_then_eval [] (extend_env (extend_env (extend_env ([],[]) "F" (SymAtom "F")) "T" (SymAtom "T")) "otherwise" (SymAtom "T")) defs expr



--
-- Evaluators
--

sl_eval :: SmLispExpr -> FEnv -> Env -> Maybe SExpression
sl_eval (FnCall i exprs) fnenv valenv =
  case (sl_evlis exprs fnenv valenv) of
    listofmaybes -> sl_apply i (map fromJust listofmaybes) fnenv (reset_to_global_frame valenv)
    _ -> Just (SymAtom "failure at eval")
sl_eval (CondExpr e) fnenv valenv = sl_evcond (CondExpr e) fnenv valenv
sl_eval (LetExpr ldefs expr) fnenv valenv =
  case (extend_local_env fnenv valenv (LetExpr ldefs expr)) of
    Just env -> sl_eval expr fnenv env
    _ -> Nothing
sl_eval (Variable e) fnenv valenv = sl_apply_env valenv e
sl_eval (SExpr (NumAtom e)) fnenv valenv = Just (NumAtom e)
sl_eval (SExpr (SymAtom e)) fnenv valenv = Just (SymAtom e)
sl_eval (SExpr (List e)) fnenv valenv =Just (List e)
sl_eval s fnenv valenv = Just (SymAtom "failure at eval")

sl_evlis :: [SmLispExpr] -> FEnv -> Env -> [Maybe SExpression]
--i use flip here to allow application of map
sl_evlis exprs fnenv valenv = map (flip ((flip sl_eval) fnenv) valenv) exprs

-- left in some debugging stuff
sl_evcond :: SmLispExpr -> FEnv -> Env -> Maybe SExpression
sl_evcond (CondExpr ((pred, result):clauses)) fnenv env =
  case (sl_eval pred fnenv env) of
    Just (SymAtom "T") -> sl_eval result fnenv env
    Just (SymAtom "F") -> sl_evcond (CondExpr clauses) fnenv env
    Just e -> Just e 
    _ -> Just (SymAtom "failure at evcond 1")
sl_evcond (CondExpr ((pred, result):[])) fnenv env = Nothing
sl_evcond (CondExpr (c:clauses)) fnenv env =
  case (sl_evcond (CondExpr clauses) fnenv env) of
    Just e -> Just e
    _ -> Just (SymAtom "failure at evcond 2")





--
-- Apply
--


sl_apply :: Identifier -> [SExpression] -> FEnv -> Env -> Maybe SExpression
sl_apply name (a1:args) fnenv valenv 
  | name == "first"   = sl_first a1
  | name == "rest"    = sl_rest a1
  | name == "endp"    = sl_endp a1
  | name == "numberp" = sl_numberp a1
  | name == "symbolp" = sl_symbolp a1
  | name == "listp"   = sl_listp a1
  | name == "explode" = sl_explode a1
  | name == "implode" = sl_implode a1
sl_apply name (a1:a2:args) fnenv valenv 
  | name == "eq"        = sl_eq a1 a2
  | name == "cons"      = sl_cons a1 a2
  | name == "plus"      = sl_plus a1 a2
  | name == "minus"     = sl_minus a1 a2
  | name == "times"     = sl_times a1 a2
  | name == "divide"    = sl_times a1 a2
  | name == "rem"       = sl_rem a1 a2
  | name == "eqp"       = sl_eqp a1 a2
  | name == "lessp"     = sl_lessp a1 a2
  | name == "greaterp"  = sl_greaterp a1 a2
sl_apply name args fnenv valenv =
  case (sl_apply_fn_env fnenv name) of
    Just (FunctionDef na param exp) -> sl_eval exp fnenv (add_associations valenv param args)
    _ -> Just (SymAtom "failure at apply")



--
-- Main
--

main = do
  fileNames <- getArgs
  --something i found on stack exchange to read multiple files
  srcText <- (fmap concat . mapM readFile) fileNames

-- print out the parsed files to show that it actually works
  case parse_main(srcText) of
    Just p -> hPutStrLn stdout (show p)
    _ -> hPutStrLn stdout "Parse failure"


-- example interpretation
-- this example is meant to work with the differentiator
-- :main deriv.sl algexprs.adt
  hPutStrLn stdout "deriv test (fails if not loading deriv.sl and algexprs.adt)"
  case parse_main(srcText) of
    Just p ->
      case (interpret p (FnCall "deriv" [ (SExpr (SymAtom "y")), (SExpr (SymAtom "x")) ] ) ) of 
        Just res -> hPutStrLn stdout (show res)
        _ -> hPutStrLn stdout "Interpreter failure"
    _ -> hPutStrLn stdout "Parse failure"

--i would test the small lisp interpreter, but this assignment is already late
