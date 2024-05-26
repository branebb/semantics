data Term = Var Int | Lam Term | App Term Term
  deriving (Show, Read, Eq)

shift :: Int -> Term -> Term
shift d t = f 0 t
  where f c v@(Var x) = if x >= c then Var (d + x) else v
        f c (Lam t) = Lam $ f (c + 1) t
        f c (App t u) = App (f c t) (f c u)

-- Primjer (\.\.2 1 (\.3))  
primjer1 = shift 2 (Lam (Lam (App (App (Var 2) (Var 1)) (Lam (Var (3))))))
--

subst :: Int -> Term -> Term -> Term
subst j s t = f 0 t
  where f c v@(Var x) = if j + c == x then shift c s else v
        f c (Lam t) = Lam $ f (c + 1) t
        f c (App t u) = App (f c t) (f c u)

-- Primjer [1 -> \.0 1] (\.\.3 0)
primjer2 = subst 1 (Lam (App (Var 0) (Var 1))) (Lam (Lam (App (Var 3) (Var 0))))
--

beta :: Term -> Term -> Term
beta s t = shift (-1) $ subst 0 (shift 1 s) t

-- Primjer (\.0) (\.1)
primjer3 = beta (Lam (Var 1)) (Var 0)
--

eval_step :: Term -> Term
eval_step (App (Lam t) u) = beta u t
eval_step (App t u) = App (eval_step t) (eval_step u)
eval_step (Lam t) = Lam $ eval_step t
eval_step t = t

eval :: Term -> Term
eval t = if t == t' then t else eval t'
  where t' = eval_step t

-- Primjer (\.(\.0 1) 0) (\.0)
primjer4 = eval (App (Lam (App (Lam (App (Var 0) (Var 1))) (Var 0))) (Lam (Var 0)))
--

i2c :: Int -> Term
i2c n = Lam $ Lam $ f n
  where f 0 = Var 0
        f n = App (Var 1) (f (n - 1))

c2i :: Term -> Int
c2i (Lam (Lam t)) = f 0 t
  where f n (Var 0) = n
        f n (App (Var 1) t) = f (n + 1) t
        f _ _ = -1
c2i _ = -1

-- INTERPRETER
s2t_var :: String
s2t_var = ['0'..'9']

s2t_lam :: String
s2t_lam = "\\."

s2t_sym :: String
s2t_sym = "()="

s2t_idt :: String
s2t_idt = ['a' .. 'z'] ++ ['0'..'9']

t2s (Var x) = show x
t2s (Lam t) = s2t_lam ++ t2s t
t2s (App s t@(App _ _)) = t2s s ++ " (" ++ t2s t ++ ")"
t2s (App s t@(Lam _)) = t2s s ++ " (" ++ t2s t ++ ")"
t2s (App s@(Lam _) t) = "(" ++ t2s s ++ ") " ++ t2s t
t2s (App s t) = t2s s ++ " " ++ t2s t

s2t_static_mem :: [(String, Term)]
s2t_static_mem = 
       [("tru", Lam (Lam (Var 1))), ("fls", Lam (Lam (Var 0))),
       ("and", Lam (Lam (App (App (Var 1) (Var 0)) (Lam (Lam (Var 0)))))),  
       ("or",  Lam (Lam (App (App (Var 1) (Lam (Lam (Var 1)))) (Var 0)))),
       ("not", Lam (App (App (Var 0) (Lam (Lam (Var 0)))) (Lam (Lam (Var 1))))),
       ("scc", Lam (Lam (Lam (App (Var 1) (App (App (Var 2) (Var 1)) (Var 0)))))),
       ("plus", Lam (Lam (Lam (Lam (App (App (Var 3) (Var 1)) (App (App (Var 2) (Var 1)) (Var 0))))))),
       ("pair", Lam (Lam (Lam (App (App (Var 0) (Var 2)) (Var 1))))),
       ("fst", Lam (App (Var 0) (Lam (Lam (Var 1))))),
       ("snd", Lam (App (Var 0) (Lam (Lam (Var 0))))),
       ("nil", Lam (Lam (Var 0))),
       ("cons", Lam (Lam (Lam (Lam (App (App (Var 1) (Var 3)) (App (App (Var 2) (Var 1)) (Var 0)))))))]
       ++ [('c' : show i, i2c i) | i <- [0 .. 1000]]

s2t_set_mem :: [(String, Term)] -> String -> Term -> [(String, Term)]
s2t_set_mem (p@(a, b) : m) s t = 
  if a == s then (a, t) : m else p : s2t_set_mem m s t
s2t_set_mem [] s t = [(s, t)] 

s2t_get_mem :: [(String, Term)] -> String -> [Term]
s2t_get_mem ((a, b) : m) s = 
  if a == s then [b] else s2t_get_mem m s
s2t_get_mem [] _ = []

-- Primjer
primjer5 = do let m0 = s2t_static_mem
              let m1 = s2t_set_mem m0 "f" (Lam (Lam (App (Var 0) (Var 1))))
              putStr $ t2s $ head $ s2t_get_mem m1 "f"
              putStr "\n"
              putStr $ show $ s2t_get_mem m1 "gg"
              putStr "\n"
              let m2 = s2t_set_mem m1 "tru" (Lam (Var 0))
              putStr $ t2s $ head $ s2t_get_mem m2 "tru"
              putStr "\n"
--

s2t_read_var :: String -> (String, String)
s2t_read_var s = f ("", s)
  where f (t, s@(c : cs)) = if c `elem` s2t_var then f (c : t, cs) else (reverse t, s)
        f (t, "") = (reverse t, "")
        
-- Primjer
primjer6 = s2t_read_var "12b3 csmxcmv smlgslmgslm"
--

s2t_read_lam :: String -> (String, String)
s2t_read_lam s
  | h == s2t_lam = (h, drop l s)
  | otherwise = ("", s)
  where h = take l s
        l = length s2t_lam
 
-- Primjer
primjer7 = s2t_read_lam "\\.\\.0 1"
--

s2t_read_idt :: String -> (String, String)
s2t_read_idt s = f ("", s)
  where f (t, s@(c : cs)) = if c `elem` s2t_idt then f (c : t, cs) else (reverse t, s)
        f (t, "") = (reverse t, "")

-- Primjer
primjer8 = s2t_read_idt "a123b( d fsdg gsg sz57 776 r6"
--

s2t_tokenize :: String -> [(String, String)] -> [(String, String)]
s2t_tokenize s@(c : cs) t
  | c == ' ' = s2t_tokenize cs t
  | c `elem` s2t_var = if v == [] then [] else s2t_tokenize vs ((v, "var") : t)
  | c == head s2t_lam = if l == [] then [] else s2t_tokenize ls ((l, "lam") : t)
  | c `elem` s2t_sym = s2t_tokenize cs (([c], "sym") : t)
  | c `elem` s2t_idt = if i == [] then [] else s2t_tokenize is ((i, "idt") : t)
  | otherwise = []
  where (v, vs) = s2t_read_var s
        (l, ls) = s2t_read_lam s
        (i, is) = s2t_read_idt s
s2t_tokenize "" t = reverse t

-- Primjer
primjer9 = s2t_tokenize "(\\.\\.\\.\\.(3 1) (2 1 0)) c4 (\\.\\.(1 0))" []
--

-- Sljedece funkcije primaju 
--      memoriju (s2t_static_mem sa dodatnim pridruzenim vrijednostima) 
--      listu tokena
-- Funkcije vracaju 
--      ([Interpretirani term], [Ostatak liste tokena]) - uspjeh
--      ([], []) - pogreska

-- Interpretira term oblika
--      (znamenka iz s2t_var)*
s2t_interpret_var :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_var m ((n, "var") : s) = 
  if all (`elem` s2t_var) n then ([Var (read n :: Int)], s) else ([], [])

-- Pomocna tail-rekurzivna funkcija, interpretira term oblika
--      term*
s2t_interpret_lst :: [(String, Term)] -> [(String, String)] -> [Term] -> ([Term], [(String, String)])
s2t_interpret_lst m s@((")", "sym") : _) l = (reverse l, s)
s2t_interpret_lst m [] l = (reverse l, [])
s2t_interpret_lst m s l = if t' == [] then ([], []) else s2t_interpret_lst m s' (t' ++ l)
  where (t', s') = s2t_interpret_term m s

-- Interpretira term oblika kao niz lijevo asocijativnih primjena terma App
--      "(" + term* + ")"
s2t_interpret_grp :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_grp m (("(", "sym") : s)
  | l == [] || s' == [] = ([], [])
  | head s' == (")", "sym") = ([foldl (\a e -> App a e) (head l) (tail l)], tail s')
  | otherwise = ([], [])
  where (l, s') = s2t_interpret_lst m s []

-- Interpretira term oblika:
--      "\." + term*
s2t_interpret_lam :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_lam m ((s2t_lam, "lam") : s)
  | l /= [] = ([Lam (foldl (\a e -> App a e) (head l) (tail l))], s')
  | otherwise = ([], [])
  where (l, s') = s2t_interpret_lst m s []

-- Interpretira term oblika
--      (znak iz s2t_idt)*
-- koji mora postojati u memoriji
s2t_interpret_idt :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_idt m ((i, "idt") : s) = if t == [] then ([], []) else (t, s)
  where t = s2t_get_mem m i

-- Term moze biti
--      primjena terma na term u zagradama -> s2t_interpret_grp
--      varijabla -> s2t_interpret_var
--      lambda funkcija -> s2t_interpret_lam
--      identifier -> s2t_interpret_idt
s2t_interpret_term :: [(String, Term)] -> [(String, String)] -> ([Term], [(String, String)])
s2t_interpret_term m s@((a, b) : _)
  | a == "(" && b == "sym" = s2t_interpret_grp m s
  | b == "var" = s2t_interpret_var m s
  | b == "lam" = s2t_interpret_lam m s
  | b == "idt" = s2t_interpret_idt m s
  | otherwise = ([], []) 

-- Interpretiramo izraze oblika
--      identifier = term
-- Koristeci s2t_set_mem spremamo u memoriju par ("identifier", term)
-- U slucaju krivog unosa vracamo [], inace vracamo azuriranu memoriju
s2t_interpret_assign :: [(String, Term)] -> [(String, String)] -> [(String, Term)]
s2t_interpret_assign m ((a, "idt") : ("=", "sym") : t) = 
  if l == [] || s /= [] then [] else s2t_set_mem m a (head l)
  where (l, s) = s2t_interpret_term m (("(", "sym") : t ++ [(")", "sym")])

s2t_interpreter :: [(String, Term)] -> (Term -> String) -> IO ()
s2t_interpreter m f =
  do putStr ">> "
     l <- getLine
     if l == [] then do
       s2t_interpreter m f
     else if l == "-h" || l == "-help" then do
       putStr "Commands:\n"
       putStr "-lambda: lambda notation\n"
       putStr "-int: integer notation\n"
       putStr "-raw: raw output\n"
       putStr "-exit: shut down the interpreter\n"
       putStr "-clear: clear screen\n"
       s2t_interpreter m f
     else if l == "-lambda" then do
       putStr "Setting lambda notation.\n"
       s2t_interpreter m t2s
     else if l == "-int" then do
       putStr "Setting integer notation.\n"
       s2t_interpreter m (\x -> let n = c2i x in if n == -1 then "NaN" else 'c' : show n)
     else if l == "-raw" then do
       putStr "Switching to raw output.\n"
       s2t_interpreter m (\x -> show x)
     else if l == "-exit" then do
       putStr "The end.\n"
       return ()
     else if l == "-clear" then do
       putStr $ replicate 72 '\n'
       s2t_interpreter m f
     else if head l == '-' then do
       putStr $ "Unrecognized command.\n"
       s2t_interpreter m f
     else do
       let s = s2t_tokenize l []
       if s == [] then do
         putStr "Invalid input.\n"
         s2t_interpreter m f
       else if ("=", "sym") `elem` s then do
         let n = s2t_interpret_assign m s
         if n == [] then do
           putStr "Invalid input.\n"
           s2t_interpreter m f
         else do
           putStr $ (fst . head) s ++ " defined.\n"
           s2t_interpreter n f
       else do
         let (t, r) = s2t_interpret_term m (("(", "sym") : s ++ [(")", "sym")])
         if t == [] || r /= [] then do
           putStr "Invalid input.\n"
         else do
           let p = (f . eval . head) t
           if p /= "" then do
             putStr $ p ++ "\n"
           else do
             putStr $ (t2s . eval . head) t ++ "\n"
         s2t_interpreter m f
                   
main = do putStr "Haskell interpreter of untyped lambda calculus.\n"
          putStr "Input -h for help, exit to terminate the program.\n"
          s2t_interpreter s2t_static_mem t2s   