--module RegEx (*) where
import Data.List
import Data.Char
import Test.HUnit


data RegEx = Sym Char
           | Seq RegEx RegEx
           | Alt RegEx RegEx
           | Rep RegEx
  deriving Show

data NFA = NFA NFANode

data NFAEdge = NFAEdge Char NFANode | Epsilon NFANode
  deriving (Show, Eq)

data NFANode = NFANode [NFAEdge] Bool
  deriving (Show, Eq)

type RegString = String
type RegResult = Bool

isEndState :: NFANode -> Bool
isEndState (NFANode _ b) = b

isEpsilon :: NFAEdge -> Bool
isEpsilon (NFAEdge _ _) = False
isEpsilon (Epsilon _) = True

edgeNode :: NFAEdge -> NFANode
edgeNode (NFAEdge _ node) = node
edgeNode (Epsilon node) = node

nodeEdges :: NFANode -> [NFAEdge]
nodeEdges (NFANode xs _) = xs

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
compile rs str = True

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
          result = runNFA (makeNFA re) str

makeNFA :: RegEx -> NFANode
makeNFA r = makeNFANode r (NFANode [] True)

makeNFANode :: RegEx -> NFANode -> NFANode
makeNFANode (Sym c) next = NFANode [NFAEdge c next] False
makeNFANode (Seq a b) next = NFANode [Epsilon $ makeNFANode a bNode] False
    where   bEdge = Epsilon bNode
            bNode = makeNFANode b next
makeNFANode (Alt a b) next = NFANode [aEdge, bEdge] False
    where   aEdge = Epsilon $ makeNFANode a next
            bEdge = Epsilon $ makeNFANode b next
makeNFANode (Rep a) next = let branchNode = NFANode [Epsilon next, Epsilon rNode] False
                               rNode = makeNFANode a branchNode       
                           in  branchNode

runNFA :: NFANode -> String -> Bool
runNFA node [] = isEndState node
runNFA node (c:rest) = let nextStates = nextNodes node c
                           nextResults = map (flip runNFA rest) nextStates
                       in  any id nextResults

nextNodes :: NFANode -> Char -> [NFANode]
nextNodes node c = map edgeNode (filter (validEdge c) allEdges)
    where   validEdge a (NFAEdge c _) = a == c
            validEdge _ (Epsilon _) = False
            allEdges = nub $ concat (map nodeEdges (allPossible node))

allPossible :: NFANode -> [NFANode]
allPossible node@(NFANode edges _) = node : concat (map allPossible epNodes)
    where   epEdges = filter isEpsilon edges
            epNodes = map edgeNode epEdges
