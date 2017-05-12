module Main where

import           Control.Exception
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Lens.Micro.Platform
import           Message_QCA
import           System.Exit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Types.Messages

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
              [ testCase "no messages"
                    $ 0 @=? countMessages noMessages
              , testProperty "has messages"  -- n.b. silly impl. for now
                    $ \x -> not (Seq.null (x :: Messages)) ==>
                            0 /= countMessages x
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
moveDownTestEmpty = testProperty "Move up in empty messages" $
                    \x -> Nothing == getNextPostId x noMessages

moveUpTestEmpty :: TestTree
moveUpTestEmpty = testProperty "Move down in empty messages" $
                    \x -> Nothing == getPrevPostId x noMessages

moveDownTestSingle :: TestTree
moveDownTestSingle = testProperty "Move up from single message" $
                   \x -> let msgs = appendMessage x noMessages
                         in Nothing == (getNextPostId (x^.mPostId) msgs)

moveUpTestSingle :: TestTree
moveUpTestSingle = testProperty "Move down from single message" $
                    \x -> let msgs = appendMessage x noMessages
                          in Nothing == (getPrevPostId (x^.mPostId) msgs)

postids :: Show a => Seq.Seq (a, Message) -> String
postids nms = intersperse ", " $ fmap pid nms
    where pid (n, m) = show n <> ".mPostID=" <> show (m^.mPostId)
          intersperse b ls = case Seq.viewl ls of
                               Seq.EmptyL -> ""
                               l Seq.:< r -> l <> b <> (intersperse b r)

uniqueIds :: Seq.Seq Message -> Bool
uniqueIds msgs =
    let ids = foldr (\m s -> Set.insert (m^.mPostId) s) Set.empty msgs
    in  Seq.length msgs == Set.size ids

moveDownTestMultipleStart :: TestTree
moveDownTestMultipleStart =
    testProperty "Move down in multiple messages from the start" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getNextPostId ((postMsg x)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                (postMsg y)^.mPostId == msgid

moveUpTestMultipleStart :: TestTree
moveUpTestMultipleStart =
    testProperty "Move up in multiple messages from the start" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getPrevPostId ((postMsg x)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

moveDownTestMultipleEnd :: TestTree
moveDownTestMultipleEnd =
    testProperty "Move down in multiple messages from the end" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                                    -- n.b. makes more sense to start at z...
                             msgid = getNextPostId ((postMsg z)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

moveUpTestMultipleEnd :: TestTree
moveUpTestMultipleEnd =
    testProperty "Move up in multiple messages from the end" $
                     \(x, y, z) ->
                         let msgs = Seq.fromList [ postMsg x
                                                 , postMsg y
                                                 , postMsg z]
                             msgid = getPrevPostId ((postMsg z)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "xyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $
                                ((postMsg y)^.mPostId) == msgid

moveDownTestMultipleSkipDeleted :: TestTree
moveDownTestMultipleSkipDeleted =
    testProperty "Move down in multiple messages skipping deleteds" $
                     \(w, x, y, z) ->
                         let msgs = Seq.fromList [ postMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , postMsg z]
                             msgid = getNextPostId ((postMsg w)^.mPostId) msgs
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
                             msgid = getPrevPostId ((postMsg z)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $
                                ((postMsg w)^.mPostId) == msgid

moveDownTestMultipleSkipDeletedAll :: TestTree
moveDownTestMultipleSkipDeletedAll =
    testProperty "Move one down in multiple deleted messages skipping deleteds" $
                     \(w, x, y, z) ->
                         -- n.b. current selected is also deleted,
                         -- which can happen due to multi-user async
                         -- server changes.
                         let msgs = Seq.fromList [ delMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , delMsg z]
                             msgid = getNextPostId ((delMsg w)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid

moveUpTestMultipleSkipDeletedAll :: TestTree
moveUpTestMultipleSkipDeletedAll =
    testProperty "Move one up in multiple deleted messages skipping deleteds" $
                     \(w, x, y, z) ->
                         -- n.b. current selected is also deleted,
                         -- which can happen due to multi-user async
                         -- server changes.
                         let msgs = Seq.fromList [ delMsg w
                                                 , delMsg x
                                                 , delMsg y
                                                 , delMsg z]
                             msgid = getPrevPostId ((delMsg z)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid
