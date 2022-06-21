--Projekt FLP1 - plg2nka - 2021
--autor: Havlicek Lukas (xhavli46)

import System.IO
import System.Environment
import Data.List
import Data.Maybe

--rozdeleni pravidla na levou a pravou cast
data Rule = Rule { left :: String
                 , right :: String} deriving (Show, Eq)

--vstupni gramatika (a transformovana)
data PLG = PLG { nonterminals :: [String]
               , terminals :: [String]
               , first_nonterminal :: Char
               , rules :: [Rule]} deriving (Show)

--pravidla automatu (ze ktereho stavu, do ktereho stavu, pomoci ktereho znaku)
data FSMrule = FSMrule { from :: Int
                       , to :: Int
                       , symbol :: Char} deriving (Show)

--vyledny automt
data FSM = FSM { states :: [Int]
               , symbols :: String
               , first_state :: Int
               , final_states :: [Int]
               , fsm_rules :: [FSMrule]} deriving (Show)

--nahradi carky v retezci za mezery
replace_comma_space :: String -> String
replace_comma_space[] = []
replace_comma_space (x:xs)
    | x == ',' = ' ' : replace_comma_space xs
    | otherwise = x : replace_comma_space xs

--nahradi prvni sipku v retezci za mezeru
replace_arrow_space :: String -> String
replace_arrow_space [] = []
replace_arrow_space (x:y:xs)
    | (x:y:[]) == "->" = ' ' : xs
    | otherwise = x : replace_arrow_space (y:xs)
replace_arrow_space x = x

--zda je charakter epsiol
isEpsilon :: Char -> Bool
isEpsilon char = char == '#'

--zda terminal vyhovuje zadani (a-z)
isValidTerminal :: Char -> Bool
isValidTerminal char = elem char ['a'..'z']

--zda neterminal vyhovuje zadani (A-Z)
isValidNonterminal :: Char -> Bool
isValidNonterminal char = elem char ['A'..'Z']

--kontrola zda list obsahuje duplicity
containsDuplicate :: (Eq a) => [a] -> Bool
containsDuplicate [] = False
containsDuplicate (x:xs)
    | elem x xs = True
    | otherwise = containsDuplicate xs

--kontroluje pravou cast pravidel PLG (jestli zacina pouze terminaly a pokracuje neterminaly) a jestli terminal je z mnoziny terminalu
checkRight :: String -> [String] -> [String] -> Bool
checkRight [] _ _ = True
checkRight (x:xs) nonterminals terminals
    | isValidTerminal x && elem (x:[]) terminals = checkRight xs nonterminals terminals
    | otherwise = checkRightNonterminal (x:xs) nonterminals

--kontroluje pravou cast pravidel PLG (jestli konci pouze neterminalem) a jestli neterminal je z mnoziny neterminalu
checkRightNonterminal :: String -> [String] -> Bool
checkRightNonterminal [] _ = True
checkRightNonterminal (x:xs) nonterminals
    | isValidNonterminal x && xs == [] && elem (x:[]) nonterminals = True
    | otherwise = False

-- kontrola pravidla, ze leva cast je neterminal a prava je xB nebo x (kde x je sigma star a B je nonterm)
-- a kontrola neterminalu a terminulu ze jsou z mnozin gramatiky
checkRule :: Rule -> [String] -> [String] -> Bool
checkRule (Rule left "#") nonterminals _ = isValidNonterminal nonterm && elem nonterm_as_str nonterminals
    where 
        nonterm = (head left)
        nonterm_as_str = ((head left):[])--protoze pole je pole stringu
checkRule (Rule left right)  nonterminals terminals = 
    isValidNonterminal nonterm && elem nonterm_as_str nonterminals && checkRight right nonterminals terminals && isValidTerminal (head right)
    where 
        nonterm = (head left)
        nonterm_as_str = ((head left):[])--protoze pole je pole stringu

--zaroven kontrola ze obsahuje prave 1x "->" a max 1 znak na leve strane
createRule :: String -> Rule
createRule str
    | ((length arr) == 2) && (length (head arr) == 1) = Rule (head arr) (last arr)
    | otherwise = error "Spatne specifikovane pravidlo ve vstupu"
    where
        arr = words $ replace_arrow_space str

--kontrola zde pravidlo splnuje pozadavky pro transformovanou gramatiku (nemusi se transformovat, jiz je ve spravnem tvaru)
isRightRule :: Rule -> Bool
isRightRule (Rule left "#") = True
isRightRule (Rule left (r:rs)) = isValidTerminal r && length rs == 1 && isValidNonterminal (head rs)
isRightRule _ = False

--tisknuti gramatiky dle zadani (pro prepinac -i a -1)
printPLG (PLG nonterminals terminals first_nonterminal rules) = do
    putStrLn $ intercalate "," nonterminals
    putStrLn $ intercalate "," terminals
    putStrLn (first_nonterminal:[])
    mapM_ putStrLn [left x ++"->"++right x | x <- rules]

--provede transformaci pravidla dle vety 3.2 (TIN)
--funkce vraci (nova pravidla, nove neterminaly, pole pro cislovani)
transformRule :: (Rule,[Int]) -> ([Rule],[String],[Int])
transformRule ((Rule left ""), int_arr) = ([Rule left "#"],[], int_arr)
transformRule (rule@(Rule left right), int_arr)
    | isRightRule rule = ([rule],[], int_arr)
    | isValidTerminal $ fst_right = ((Rule left (fst_right:new_nonterm)):other_rules,new_nonterm:new_nonterms,other_array)
    where
        fst_right = head right
        next_num = head int_arr
        next_arr = drop 1 int_arr
        new_nonterm = (head left):(show next_num)
        (other_rules,new_nonterms,other_array) = transformRule ((Rule new_nonterm (tail right)),next_arr)

--provede tranformaci vsech pravidel (viz funkce nad touto) + si predava pole cisel pro cislovani novych neterminalu
--funkce vraci (transformovana pravidla, nove neterminaly)
transformRules :: [Rule] -> [Int] -> ([Rule],[String])
transformRules [] _ = ([],[])
transformRules (r:rs) int_arr = (transformed_rules ++ rec_rules,new_nonterms ++ rec_new_nonterms)
    where
        (rec_rules,rec_new_nonterms) = transformRules rs new_arr
        (transformed_rules,new_nonterms,new_arr) = transformRule (r,int_arr)

--pro kazdy neterminal je stav automatu
generateFsmStates :: [String] -> [Int]
generateFsmStates rules = [1..(length rules)]

--prevedeni terminalu jako string na char (pomoci head) a spojeni je v 1 string
generateFsmSymbols :: [String] -> String
generateFsmSymbols [] = []
generateFsmSymbols (x:xs) = (head x) : generateFsmSymbols xs 

--ziskani stavu vuci neterminalu (neterminaly jsou prevedeny na pole)
getStateFromNonterm :: String -> [String] -> Int
getStateFromNonterm nonterminal nonterminals = (fromJust $ elemIndex nonterminal nonterminals)+1

--ziskani finalnich stavu (vsechny stavy, ktera jsou z neterminalu z epsilon pravidel)
getFinalStates :: [Rule] -> [String] -> [Int]
getFinalStates [] _ = [] 
getFinalStates (r:rs) nonterminals
    | isEpsilon $ head $ right r = (getStateFromNonterm (left r) nonterminals) : getFinalStates rs nonterminals --head kvuli string a char konverzi
    | otherwise = getFinalStates rs nonterminals

--tranformace pravidel gramatiky na pravidla automatu
createFSMrules :: [Rule] -> [String] -> [FSMrule]
createFSMrules [] _ = []
createFSMrules (r:rs) nonterminals
    | length (right r) > 1 = (FSMrule from to symbol) : createFSMrules rs nonterminals
    | otherwise = createFSMrules rs nonterminals --pokud je delka prave strany kratsi nez 1, jedna se o eps pravidlo a pro ty se pravidla do FSM nepridavaji
    where
        from = getStateFromNonterm (left r) nonterminals
        to = getStateFromNonterm (tail $ right r) nonterminals
        symbol = head $ right r

--transformace PGL na FSM (pomoci ostatnich funkci)
createFSMfromPLG :: PLG -> FSM
createFSMfromPLG (PLG nonterminals terminals first_nonterminal rules) = 
    FSM (generateFsmStates nonterminals) (generateFsmSymbols terminals) (getStateFromNonterm (first_nonterminal:[]) nonterminals) (getFinalStates rules nonterminals) (createFSMrules rules nonterminals)

--vytisk automatu dle zadani (pro prepinac -2)
printFSM (FSM states symbols first_state final_states fsm_rules) = do
    putStrLn $ intercalate "," [show x | x <- states]
    putStrLn symbols
    putStrLn $ show first_state
    putStrLn $ intercalate "," [show x | x <- final_states]
    mapM_ putStrLn [show (from x) ++","++ (symbol x) :","++ show (to x) | x <- fsm_rules]

--main
main = do 
    args <- getArgs

    if (null args) then
        error "Program musi byt spusten s parametry"
    else return ()
        
    --nacteni vstupu ze stdin nebo ze souboru podle poctu argumentu
    fileContent <- if ((length args) == 1 ) then
        getContents
    else
        readFile $ head $ tail args

    let idk = head $ error "test"
    
    let fileContent_lines = lines fileContent
    --print fileContent_lines

    
    if ((length fileContent_lines) < 3) then
        error "Nespravny vstup programu (vstup je prilis kratky)"
    else return ()

    let nonterminals = words $ replace_comma_space $ head fileContent_lines 
    let terminals =  words $ replace_comma_space $ head $ tail fileContent_lines
    let first_nonterminal = head $ tail $ tail fileContent_lines
    let rules = [createRule x | x <- tail $ tail $ tail fileContent_lines]
    let check = [checkRule x nonterminals terminals | x <- rules]
    -- kontroly ze terminaly a neterminaly jsou jen 1 znak a neobsahuji duplicity (jsou to mnoziny)
    if all (\x -> length x == 1 && isValidNonterminal (head x)) nonterminals && not (containsDuplicate nonterminals) then
        return ()
    else error "Spatne specifikovane neterminaly"

    if all (\x -> length x == 1 && isValidTerminal (head x)) terminals && not (containsDuplicate terminals) then
        return ()
    else error "Spatne specifikovane terminaly"

    --kontrola ze pocatecni neterminal je obsazen v mnozine neterminalu
    if (length first_nonterminal) == 1 && isValidNonterminal (head first_nonterminal) && elem first_nonterminal nonterminals then
        return ()
    else error "Spatny format pocatecniho neterminalu"

    if all (==True) check && not (containsDuplicate rules) then
        return ()
    else error "Spatny format pravidel"


    let plg = PLG nonterminals terminals (head first_nonterminal) rules

    let transformed_rules_nonterms = transformRules rules [1..]
    let transformed_plg = PLG (nonterminals ++ snd transformed_rules_nonterms) terminals (head first_nonterminal) (fst transformed_rules_nonterms)

    let fsm = createFSMfromPLG transformed_plg

    case (head args) of "-i" -> printPLG plg
                        "-1" -> printPLG transformed_plg
                        "-2" -> printFSM fsm
                        _ -> error "Prvni parametr musi byt: -i, -1 nebo -2"