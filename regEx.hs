module RegEx (makeNFA) where
import Data.List
import Data.Char



data RegEx = Sym Char
           | Seq RegEx RegEx
           | Alt RegEx RegEx
           | Rep RegEx

data NFA = NFA NFANode

data NFAEdge = NFAEdge Char NFANode | Epsilon NFANode
  deriving Show

data NFANode = NFANode [NFAEdge] Bool
  deriving Show

type RegString = String
type RegResult = Bool
data TestCase = TestCase RegString String Bool
  deriving Show

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

tests :: [TestCase]
tests = [
    TestCase "a" "a" True,
    TestCase "a" "" False,
    TestCase "a" "b" False,
    TestCase "b" "a" False,
    TestCase "b" "b" True,
    TestCase "a" "A" False,
    TestCase "[ab]" "a" True,
    TestCase "[ab]" "b" True,
    TestCase "[ab]" "c" False,
    TestCase "a*" "" True,
    TestCase "a*" "a" True,
    TestCase "a*" "aaaa" True,
    TestCase "a*" "b" False,
    TestCase "a*" "ab" False,
    TestCase "a*" "aab" False,
    TestCase "a-d" "c" True,
    TestCase "a-d" "ad" False,
    TestCase "a-d" "e" False,
    TestCase "a-d" "d" True
      ]

main = do
  let lengthStr = show (length tests)
  let promptStr = "\nRunning " ++ lengthStr ++ " tests..."
  putStrLn (promptStr ++ "\n")
  putStrLn testReport

testReport :: String
testReport
  | numFailed == 0 = "All tests passed!"
  | otherwise = show numFailed ++ " tests failed\n" ++ testResults
  where testResults = concat (intersperse "\n" failStrs)
        failStrs = map errorString failedTests
        numFailed = length failedTests
        failedTests = filter ((==) False . runTest) tests

errorString :: TestCase -> String
errorString (TestCase regStr str expectedResult) = message
  where message = "regstr " ++ fRegStr ++ " failed on " ++ 
                   fInput ++ " with output " ++ fOutput
        fRegStr = ['"'] ++ regStr ++ ['"']
        fInput = ['"'] ++ str ++ ['"']
        fOutput = ['"'] ++ show expectedResult ++ ['"']

runTest :: TestCase -> Bool
runTest (TestCase regstr str result) = compile regstr str == result

compile :: RegString -> String -> RegResult
compile rs str = True

testInput :: RegEx -> String -> Bool
testInput = undefined

addEdge :: NFANode -> NFANode -> NFANode
addEdge (NFANode xs b) newNode = NFANode (newEdge:xs) b
    where   newEdge = Epsilon newNode

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
nextNodes (NFANode xs _) c = map edgeNode (filter validEdge c allEdges)
    where   validEdge a (NFAEdge c _) = a == c
            validEdge _ (Epsilon _) = True
            allEdges = concat (map nodeEdges allPossible)

allPossible :: NFANode -> [NFANode]
allPossible (NFANode edges _) = concat allPossible epNodes
    where   epEdges = filter isEpsilon edges
            epNodes = map edgeNode epEdges
