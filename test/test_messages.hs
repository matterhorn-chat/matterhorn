module Main where

import           Control.Exception
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Lens.Micro.Platform
import           Message_QCA
import           State (getNextPost, getPrevPost)
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Types.Posts

main :: IO ()
main = defaultMain tests `catch` (\e -> do
                                    if e == ExitSuccess
                                    then putStrLn "Passed"
                                    else putStrLn "FAILED"
                                    throwIO e)

tests :: TestTree
tests = testGroup "Messages Tests"
        [ createTests
        , movementTests
        ]


createTests :: TestTree
createTests = testGroup "Create"
              [ testCase "no messages" -- n.b. silly impl. for now
                    $ Seq.null Seq.empty @? "messages not null Sequence"
              , testProperty "has messages"  -- n.b. silly impl. for now
                    $ \x -> not (Seq.null (x :: Seq.Seq Message)) ==> not (Seq.null x)
              ]

movementTests :: TestTree
movementTests = testGroup "Movement"
                [ moveUpTestEmpty
                , moveUpTestSingle
                , moveUpTestMultipleStart
                , moveUpTestMultipleEnd
                , moveUpTestMultipleSkipDeleted
                , moveUpTestMultipleSkipDeletedAll
                , moveDownTestEmpty
                , moveDownTestMultipleStart
                , moveDownTestSingle
                , moveDownTestMultipleEnd
                , moveDownTestMultipleSkipDeleted
                , moveDownTestMultipleSkipDeletedAll
                ]

moveDownTestEmpty :: TestTree
moveDownTestEmpty = testCase "Move up in empty messages" $
                    Nothing @=? (getNextPost Seq.empty)

moveUpTestEmpty :: TestTree
moveUpTestEmpty = testProperty "Move down in empty messages" $
                    \x -> Nothing == (getPrevPost x Seq.empty)

moveDownTestSingle :: TestTree
moveDownTestSingle = testProperty "Move up from single message" $
                   \x -> let msgs = Seq.singleton x
                         in Nothing == (getNextPost $ Seq.drop 1 msgs)

moveUpTestSingle :: TestTree
moveUpTestSingle = testProperty "Move down from single message" $
                    \x -> Nothing == (getPrevPost (-1) $ Seq.singleton x)
                          -- n.b. the -1 there is really odd... to be fixed.

postids :: Show a => Seq.Seq (a, Message) -> String
postids nms = intersperse ", " $ fmap pid nms
    where pid (n, m) = show n <> ".mPostID=" <> show (m^.mPostId)
          intersperse b ls = case Seq.viewl ls of
                               Seq.EmptyL -> ""
                               l Seq.:< r -> l <> b <> (intersperse b r)

moveDownTestMultipleStart :: TestTree
moveDownTestMultipleStart =
    testProperty "Move down in multiple messages from the start" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getNextPost msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                (postMsg x)^.mPostId == msgid

moveUpTestMultipleStart :: TestTree
moveUpTestMultipleStart =
    testProperty "Move up in multiple messages from the start" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getPrevPost (-1) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid

moveDownTestMultipleEnd :: TestTree
moveDownTestMultipleEnd =
    testProperty "Move down in multiple messages from the end" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                                    -- n.b. makes more sense to start at z...
                             msgid = getNextPost (Seq.drop 3 msgs)
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid

moveUpTestMultipleEnd :: TestTree
moveUpTestMultipleEnd =
    testProperty "Move up in multiple messages from the end" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getPrevPost 1 msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                ((postMsg y)^.mPostId) == msgid

moveDownTestMultipleSkipDeleted :: TestTree
moveDownTestMultipleSkipDeleted =
    testProperty "Move down in multiple messages skipping deleteds" $
                     \(w, x, y, z) ->
                         let msgs = Seq.fromList [ postMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , postMsg z]
                                    -- n.b. makes more sense to start at w
                             msgid = getNextPost $ Seq.drop 1 msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                ((postMsg z)^.mPostId) == msgid

moveUpTestMultipleSkipDeleted :: TestTree
moveUpTestMultipleSkipDeleted =
    testProperty "Move one up in multiple messages skipping deleteds" $
                     \(w, x, y, z) ->
                         let msgs = Seq.fromList [ postMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , postMsg z]
                             msgid = getPrevPost 2 msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                ((postMsg w)^.mPostId) == msgid

moveDownTestMultipleSkipDeletedAll :: TestTree
moveDownTestMultipleSkipDeletedAll =
    testProperty "Move one down in multiple deleted messages skipping deleteds" $
                     \(w, x, y, z) ->
                         let msgs = Seq.fromList [ delMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , delMsg z]
                             msgid = getNextPost msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid

moveUpTestMultipleSkipDeletedAll :: TestTree
moveUpTestMultipleSkipDeletedAll =
    testProperty "Move one up in multiple deleted messages skipping deleteds" $
                     \(w, x, y, z) ->
                         let msgs = Seq.fromList [ delMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , delMsg z]
                             msgid = getPrevPost 2 msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid
