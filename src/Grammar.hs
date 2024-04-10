module Grammar where

import           Data.List (intercalate)
import           GHC.Base  (join)
import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Lexer --
data GramLexer = GramLexer String String String String String String String [RuleL]
instance Show GramLexer where
  show (GramLexer name export stored initial returned skipped code rules) = ""
    ++ "module " ++ name ++ "\n\t( tokenize" ++ export ++ " ) where\n\n"
    ++ "import Text.Regex.PCRE ( (=~) )\n"
    ++ "import Control.Monad.State\n\n"
    ++ code ++ "\n\ntype Stored = " ++ stored ++ "\ntype Returned = " ++ returned
    ++ "\ntype SimpleReturn = State Stored Returned\n"
    ++ "type ListReturn = State Stored [Returned]\n\n"
    ++ "tokenize :: String -> [Returned]\n"
    ++ "tokenize input = evalState (match regList input) (" ++ initial ++ ")\n\n"
    ++ "match :: [(String, String -> SimpleReturn)] -> String -> ListReturn\n"
    ++ "match [] \"\" = return []\n"
    ++ "match [] input = error $ \"string prefix does not match any token: \\\"\" ++ input ++ \"\\\"\"\n"
    ++ "match ((regex, getToken) : other) input =\n"
    ++ "\tcase input =~ regex :: (String, String, String) of\n"
    ++ "\t\t(\"\", current, rest) | current /= \"\" -> do\n"
    ++ "\t\t\ttoken <- getToken current\n"
    ++ "\t\t\tright <- match regList rest\n"
    ++ "\t\t\treturn ([token | token /= (" ++ skipped ++ ")] ++ right)\n"
    ++ "\t\t(_, _, _) -> match other input\n\n"
    ++ "regList :: [(String, String -> SimpleReturn)]\n"
    ++ "regList = [\n\t"
    ++ intercalate ",\n\t" (map (showRuleL skipped) rules) ++ "]\n"

data RuleL = GetToken String String
           | Skip String
showRuleL :: String -> RuleL -> String
showRuleL _ (GetToken regex code) = "(" ++ show regex ++ ", " ++ code ++ ")"
showRuleL code (Skip regex) = "(" ++ show regex ++ ", \\_ -> return (" ++ code ++ "))"


-- Parser --
data GramParser = GramParser String String String [String] [String] String [NonTerm]
instance Show GramParser where
  show (GramParser modName export tokName eTerms sTerms code nonterms) = do
    let ((NonTerm start _ _ returned _):_) = nonterms
    
    let first = calcFirst (eTerms ++ sTerms) nonterms
    let follow = calcFollow first nonterms
    unless (checkLL1 first follow nonterms) $ error $ "Not LL1 grammar\n"
    let first1 = calcFirst1 first follow nonterms Map.empty
    
    "module " ++ modName ++ "\n\t( parse" ++ export ++ " ) where\n\n"
      ++ "import Control.Monad.State\n\n"
      ++ "import Control.Monad (when)\n\n"
      ++ code ++ "\n\ntype Parser a = State [" ++ tokName ++ "] a\n\n"
      ++ "token :: Parser " ++ tokName ++ "\n"
      ++ "token = do\n\tts <- get\n\tcase ts of\n\t\t(t:ts') -> do\n"
      ++ "\t\t\tput ts'\n\t\t\treturn t\n\t\t[] -> return END\n\n"
      ++ "peek :: Parser " ++ tokName ++ "\n"
      ++ "peek = do\n\tts <- get\n\tcase ts of\n\t\t(t:_) -> return t\n"
      ++ "\t\t[] -> return END\n\n"
      ++ "parse :: [" ++ tokName ++ "] -> " ++ returned
      ++ "\nparse = evalState r" ++ start ++ "\n\n"
      ++ intercalate "\n" (map (showParseFun (Set.fromList eTerms) (Set.fromList sTerms) first1) nonterms)

data NonTerm = NonTerm String String String String [RuleP] deriving Eq
-- instance Show NonTerm where
--   show (NonTerm name typeArgs args returnType rules) = undefined

data RuleP = RuleP [(String, String)] String deriving Eq
-- instance Show RuleP where
--   show (RuleP children code) = undefined


-- First --
calcFirst :: [String] -> [NonTerm] -> Map.Map String (Set.Set String)
calcFirst terms = calcFirst' $ Map.fromList [(x, Set.singleton x) | x <- terms]

calcFirst' :: Map.Map String (Set.Set String) -> [NonTerm] -> Map.Map String (Set.Set String)
calcFirst' oldFirst grammar = let newFirst = grFirst oldFirst grammar
                              in if oldFirst == newFirst
                                then oldFirst
                                else calcFirst' newFirst grammar

grFirst :: Map.Map String (Set.Set String) -> [NonTerm] -> Map.Map String (Set.Set String)
grFirst oldFirst [] = oldFirst
grFirst oldFirst ((NonTerm x _ _ _ rules) : xs) = grFirst (ntFirst oldFirst x rules) xs

ntFirst :: Map.Map String (Set.Set String) -> String -> [RuleP] -> Map.Map String (Set.Set String)
ntFirst oldFirst _ [] = oldFirst
ntFirst oldFirst nt ((RuleP ((x, _):_) _) : xs) = ntFirst newFirst nt xs
                                                    where
                                                      s1 = Map.findWithDefault Set.empty x oldFirst
                                                      s2 = Map.findWithDefault Set.empty nt oldFirst
                                                      newFirst = Map.insert nt (s1 `Set.union` s2) oldFirst
ntFirst _ _ _ = undefined

-- Follow --
calcFollow :: Map.Map String (Set.Set String) -> [NonTerm] -> Map.Map String (Set.Set String)
calcFollow f gr@((NonTerm nt _ _ _ _):_) = calcFollow' f (Map.singleton nt (Set.singleton "END")) gr
calcFollow f _ = undefined

calcFollow' :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> [NonTerm] -> Map.Map String (Set.Set String)
calcFollow' f oldFollow grammar = if oldFollow == newFollow
                                    then oldFollow
                                    else calcFollow' f newFollow grammar
                                  where newFollow = grFollow f oldFollow grammar

grFollow :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> [NonTerm] -> Map.Map String (Set.Set String)
grFollow f oldFollow [] = oldFollow
grFollow f oldFollow ((NonTerm nt _ _ _ rules) : nts) = grFollow f (ntFollow f oldFollow nt rules) nts

ntFollow :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> String -> [RuleP] -> Map.Map String (Set.Set String)
ntFollow f oldFollow _ [] = oldFollow
ntFollow f oldFollow nt ((RuleP y _) : ys) = ntFollow f (rlFollow f oldFollow nt y) nt ys

rlFollow :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> String -> [(String, String)] -> Map.Map String (Set.Set String)
rlFollow first oldFollow nt [] = oldFollow
rlFollow first oldFollow nt [(b, _)] = rlFollow first newFollow nt []
                                        where
                                          s1 = Map.findWithDefault Set.empty b oldFollow
                                          s2 = Map.findWithDefault Set.empty nt oldFollow
                                          newFollow = Map.insert b (s1 `Set.union` s2) oldFollow
rlFollow first oldFollow nt ((b, _):y) = rlFollow first newFollow nt y
                                          where
                                            s1 = firstOfRule first y
                                            s2 = Map.findWithDefault Set.empty b oldFollow
                                            s3 = Map.findWithDefault Set.empty nt oldFollow
                                            newS = s1 `Set.union` s2 `Set.union` (if "EPS" `Set.member` s1 then s3 else Set.empty)
                                            newFollow = Map.insert b (Set.delete "EPS" newS) oldFollow

firstOfRule :: Map.Map String (Set.Set String)
       -> [(String, String)] -> Set.Set String
firstOfRule f [] = Set.singleton "EPS"
firstOfRule f ((x, _):xs) =
  let sf = Map.findWithDefault Set.empty x f
  in (Set.delete "EPS" sf) `Set.union`
    if "EPS" `Set.member` sf
      then firstOfRule f xs
      else Set.empty

-- FIRST1 --
calcFirst1 :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
           -> [NonTerm] -> Map.Map String (Map.Map [(String, String)] (Set.Set String))
           -> Map.Map String (Map.Map [(String, String)] (Set.Set String))
calcFirst1 _ _ [] f1 = f1
calcFirst1 fi fo ((NonTerm nt _ _ _ rules):nts) oldF1 = calcFirst1 fi fo nts (calcFirst1' fi fo nt oldF1 rules)

calcFirst1' :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
            -> String -> Map.Map String (Map.Map [(String, String)] (Set.Set String))
            -> [RuleP] -> Map.Map String (Map.Map [(String, String)] (Set.Set String))
calcFirst1' _ _ _ f1 [] = f1
calcFirst1' fi fo nt oldF1 ((RuleP x _):rs) =
  calcFirst1' fi fo nt newF1 rs
    where
      s1 = Map.findWithDefault Set.empty (fst (head x)) fi
      s2 = Map.findWithDefault Set.empty nt fo
      s3 = (Set.delete "EPS" s1) `Set.union` (if "EPS" `Set.member` s1 then s2 else Set.empty)
      mNt = Map.findWithDefault Map.empty nt oldF1
      newF1 = Map.insert nt (Map.insert x s3 mNt) oldF1


-- parser generator --
showParseFun :: Set.Set String -> Set.Set String -> Map.Map String (Map.Map [(String, String)] (Set.Set String)) -> NonTerm -> String
showParseFun et st f1 (NonTerm n at a t r) =
  (if a == ""
    then "r" ++ n ++ " :: Parser " ++ t ++ "\nr" ++ n
    else "r" ++ n ++ " :: " ++ at ++ " -> Parser " ++ t ++ "\nr" ++ n ++ " " ++ a)
  ++ " = do\n\tt <- peek\n\tcase t of\n"
  ++ showTokenCases et st (Map.findWithDefault Map.empty n f1) r
  ++ "\t\t_ -> error \"unexpected token\"\n"

showTokenCases :: Set.Set String -> Set.Set String -> Map.Map [(String, String)] (Set.Set String) -> [RuleP] -> String
showTokenCases et st f1 [] = ""
showTokenCases et st f1 ((RuleP r c) : xs) = showOneCase et st r c (Set.toList (Map.findWithDefault Set.empty r f1)) ++ showTokenCases et st f1 xs

showOneCase :: Set.Set String -> Set.Set String -> [(String, String)] -> String -> [String] -> String
showOneCase _ _ _ _ [] = ""
showOneCase et st r c (x:xs) = "\t\t" ++ pattern ++ " -> do\n" ++ showRule et st r 1 ++ "\t\t\t" ++ c ++ "\n" ++ showOneCase et st r c xs
                                where pattern = if (x `Set.member` st) then "(" ++ x ++ " _)" else x

showRule :: Set.Set String -> Set.Set String -> [(String, String)] -> Int -> String
showRule et st [] _ = ""
showRule et st ((x, args) : xs) i = if x == "EPS"
                                      then ""
                                      else "\t\t\tw" ++ show i ++
                                        (if x `Set.member` et
                                          then " <- token\n\t\t\twhen(w" ++ show i ++ " /= " ++ x
                                                ++ ") $ error \"unexpected token\"\n"
                                          else if x `Set.member` st
                                            then " <- token\n\t\t\tlet (" ++ x ++ " s" ++ show i ++ ") = w" ++ show i ++ "\n"
                                            else " <- r" ++ x ++ [' ' | args /= ""] ++ args ++ "\n")
                                        ++ showRule et st xs (i + 1)

-- LL1 checking --
checkLL1 :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> [NonTerm] -> Bool
checkLL1 _ _ [] = True
checkLL1 fi fo ((NonTerm nt _ _ _ rules) : xs) = if checkLL1' fi fo nt [(a, b) | (RuleP a _) <- rules, (RuleP b _) <- rules, a /= b]
                                                  then checkLL1 fi fo xs
                                                  else False

checkLL1' :: Map.Map String (Set.Set String) -> Map.Map String (Set.Set String) -> String -> [([(String, String)], [(String, String)])] -> Bool
checkLL1' _ _ _ [] = True
checkLL1' fi fo nt ((a, b) : xs) = if (fiA `Set.disjoint` fiB) && ((not ("EPS" `Set.member` fiA)) || (foNt `Set.disjoint` fiB))
                                    then checkLL1' fi fo nt xs
                                    else False
                                      where
                                        fiA = firstOfRule fi a
                                        fiB = firstOfRule fi b
                                        foNt = Map.findWithDefault Set.empty nt fo
