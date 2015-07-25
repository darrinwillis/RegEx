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

moveStart :: Move a -> a
moveStart (Move  a _ _) = a
moveStart (EMove a   _) = a

moveEnd :: Move a -> a
moveEnd (Move  _ _ b) = b
moveEnd (EMove _   b) = b

moveChar :: Move a -> Maybe Char
moveChar (Move  _ c _) = Just c
moveChar (EMove _   _) = Nothing

isEMove :: Move a -> Bool
isEMove (Move  _ _ _) = False
isEMove (EMove _   _) = True

renumber_move :: Int -> Move Int -> Move Int
renumber_move n (Move a c b) = Move (a+n) c (b+n)
renumber_move n (EMove a b) = EMove (a+n) (b+n)

flattenSet :: (Ord a) => Set(Set a) -> Set a
flattenSet = S.foldl S.union empty

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
            combineMoves = allMove (endStates n1) (start n2)
            n2start = size $ states n1
            n1 = build r1
            n2 = renumber n2start $ build r2
build (Alt r1 r2) = NFA newStates newMoves newStart $ singleton newEnd
    where   newStates = S.unions [(states n1), (states n2),
                                  fromList [newStart, newEnd]]
            newMoves = S.unions [moves n1, moves n2, combineMoves, newEndMoves]
            combineMoves = fromList [EMove newStart (start n1),
                            EMove newStart (start n2)]
            newEndMoves = S.unions [allMove (endStates n1) newEnd,
                                   allMove (endStates n2) newEnd]
            newEnd = n2end + 1
            newStart = 0
            n2start = size (states n1) + 1
            n2end = size (states n2) + n2start
            n1 = renumber 1 $ build r1
            n2 = renumber (n2start) $ build r2
build (Rep r) = NFA newStates newMoves newStart $ singleton newEnd
    where   newStates = S.union (states n) (fromList [newStart, newEnd])
            newStart = 0
            newEnd = size(states n) + 2 -- 1 for new start 1 for new end
            newMoves = S.unions [moves n, 
                    allMove (endStates n) newEnd,
                    allMove (endStates n) $ start n,
                    singleton $ EMove newStart $ start n,
                    singleton $ EMove newStart newEnd]
            n = renumber 1 $ build r

allMove :: (Ord a) => Set a -> a -> Set(Move a)
allMove starts end = S.map (flip EMove $ end) starts

runNFA :: NFA Nod -> String -> Bool
runNFA (NFA states moves start ends) str =
            processChar (singleton start) moves ends str

processChar :: (Ord a) => Set a -> Set(Move a) -> Set a -> String -> Bool
processChar starts moves ends [] = not $ S.null $ intersection allStarts ends
    where   allStarts = allConnected starts moves
processChar starts moves ends (c:rest) = processChar newStarts moves ends rest
    where   newStarts = takeMove starts moves c

takeMove :: (Ord a) => Set a -> Set(Move a) -> Char -> Set a
takeMove starts moves c = S.map moveEnd $ S.filter moveMatches theseMoves
    where   currentStates = allConnected starts moves
            moveMatches (Move st c2 _) = c == c2
            moveMatches (EMove st _) = False
            theseMoves = S.filter ((flip member) currentStates . moveStart) moves

allConnected :: (Ord a) => Set a -> Set(Move a) -> Set a
allConnected starts moves = flattenSet $ S.map (flip allCurrent moves) starts

allCurrent :: (Ord a) => a -> Set(Move a) -> Set a
allCurrent n moves =  S.insert n allConnected
    where   adjacents = takeEMove n moves
            allConnected = flattenSet $ S.map (flip allCurrent moves) adjacents

takeEMove :: (Ord a) => a -> Set(Move a) -> Set a
takeEMove n moves = S.map moveEnd $ S.filter isValid moves
    where   isValid m = and [isEMove m, (moveStart m) == n]

--nextNods :: NFANod -> Char -> [NFANod]
--nextNods node c = map edgeNod (filter (validEdge c) allEdges)
--    where   validEdge a (NFAEdge c _) = a == c
--            validEdge _ (Epsilon _) = False
--            allEdges = nub $ concat (map nodeEdges (allPossible node))

--allPossible :: NFANod -> [NFANod]
--allPossible node@(NFANod edges _) = node : concat (map allPossible epNods)
--    where   epEdges = filter isEpsilon edges
--            epNods = map edgeNod epEdges
