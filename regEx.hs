--module RegEx (*) where
import Data.List
import Data.Char
import Data.Set as S
import Test.HUnit


data RegEx = Sym Char
           | Seq RegEx RegEx
           | Alt RegEx RegEx
           | Rep RegEx
  deriving Show

data NFA a = NFA (Set a) (Set (Move a)) a (Set a)
  deriving (Show, Eq)

data Move a = Move a Char a | EMove a a
  deriving (Show, Eq, Ord)

type Nod = Int

type RegString = String
type RegResult = Bool

isEndState :: NFA Nod -> Nod -> Bool
isEndState (NFA _ _ _ ends) n = member n ends

states :: NFA a -> Set a
states (NFA st _ _ _) = st

moves :: NFA a -> Set (Move a)
moves (NFA _ mv _  _) = mv

start :: NFA a -> a
start (NFA _ _ s _) = s

endStates :: NFA a -> Set a
endStates (NFA _ _ _ e) = e

renumber :: Int -> NFA Int -> NFA Int
renumber n (NFA all moves start ends) =
        NFA (S.map func all) (newMoves) (func start) (S.map func ends)
    where   func = (+n)
            newMoves = S.map (renumber_move n) moves

renumber_move :: Int -> Move Int -> Move Int
renumber_move n (Move a c b) = Move (a+n) c (b+n)
renumber_move n (EMove a b) = EMove (a+n) (b+n)

fullTests :: [Test]
fullTests = [
    fullStackTest "a" "a" True,
    fullStackTest "a" "" False,
    fullStackTest "a" "b" False,
    fullStackTest "b" "a" False,
    fullStackTest "b" "b" True,
    fullStackTest "a" "A" False,
    fullStackTest "[ab]" "a" True,
    fullStackTest "[ab]" "b" True,
    fullStackTest "[ab]" "c" False,
    fullStackTest "a*" "" True,
    fullStackTest "a*" "a" True,
    fullStackTest "a*" "aaaa" True,
    fullStackTest "a*" "b" False,
    fullStackTest "a*" "ab" False,
    fullStackTest "a*" "aab" False,
    fullStackTest "a-d" "c" True,
    fullStackTest "a-d" "ad" False,
    fullStackTest "a-d" "e" False,
    fullStackTest "a-d" "d" True
    ]

fullStackTest :: RegString -> String -> Bool -> Test
fullStackTest rs str expected =
        TestCase $ assertEqual message (compile rs str) expected
    where message = "regstr " ++ quotify rs ++ " on input " ++
                   quotify str ++ " should be " ++ show expected

quotify :: String -> String
quotify str = '"' : str ++ ['"']

main = runTestTT $ TestList nfaTests --fullTests

compile :: RegString -> String -> RegResult
compile rs str = undefined

nfaTests :: [Test]
nfaTests = [
    nfaRunTest (Sym 'a') "a" True,
    nfaRunTest (Sym 'a') "aa" False,
    nfaRunTest (Sym 'a') "b" False,
    nfaRunTest (Sym 'a') ""  False,
    nfaRunTest (Sym 'a') "ba" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "a" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "b" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "ab" True,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "ba" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "ac" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "bb" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "bab" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "abab" False,
    nfaRunTest (Seq (Sym 'a') (Sym 'b')) "" False,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "a" True,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "b" True,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "c" False,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "ab" False,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "ba" False,
    nfaRunTest (Alt (Sym 'a') (Sym 'b')) "" False,
    nfaRunTest (Rep (Sym 'a')) "a" True,
    nfaRunTest (Rep (Sym 'a')) "" True,
    nfaRunTest (Rep (Sym 'a')) "aa" True,
    nfaRunTest (Rep (Sym 'a')) "b" False,
    nfaRunTest (Rep (Sym 'a')) "ab" False,
    nfaRunTest (Rep (Sym 'a')) "aba" False,
    nfaRunTest (Rep (Sym 'a')) "aaa" True
    ]

nfaRunTest :: RegEx -> String -> Bool -> Test
nfaRunTest re str expected = TestCase $ assertEqual message result expected
    where message = "RegEx " ++ show re ++ " on input " ++
                   quotify str ++ " should be " ++ show expected
          result = runNFA (build re) str

build :: RegEx -> NFA Nod
build (Sym c) = NFA (fromList [0, 1]) (singleton $ Move 0 c 1) 0 (singleton 1)
build (Seq r1 r2) = NFA (newStates) newMoves (start n1) (endStates n2)
    where   newStates = S.union (states n1) (states n2)
            newMoves = S.unions [moves n1, moves n2, combineMoves]
            combineMoves = S.map (flip EMove $ start n2) $ endStates n1 
            s1 = size $ states n1
            s2 = size $ states n2
            n1 = build r1
            n2 = renumber s1 $ build r2

runNFA :: NFA Nod -> String -> Bool
runNFA (NFA states moves start ends) str = undefined
--runNFA node [] = isEndState node
--runNFA node (c:rest) = let nextStates = nextNods node c
--                           nextResults = map (flip runNFA rest) nextStates
--                       in  any id nextResults

--nextNods :: NFANod -> Char -> [NFANod]
--nextNods node c = map edgeNod (filter (validEdge c) allEdges)
--    where   validEdge a (NFAEdge c _) = a == c
--            validEdge _ (Epsilon _) = False
--            allEdges = nub $ concat (map nodeEdges (allPossible node))

--allPossible :: NFANod -> [NFANod]
--allPossible node@(NFANod edges _) = node : concat (map allPossible epNods)
--    where   epEdges = filter isEpsilon edges
--            epNods = map edgeNod epEdges
