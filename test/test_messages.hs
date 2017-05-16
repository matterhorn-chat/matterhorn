{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Exception
import qualified Data.List.UniqueUnsorted as U
import           Data.Maybe (isNothing, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Lens.Micro.Platform
import           Message_QCA
import           Network.Mattermost.Types
import           System.Exit
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
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


makeMsgs :: [Message] -> Messages
makeMsgs = foldl (flip appendMessage) noMessages

idlist :: Foldable t => t Message -> [Maybe PostId]
idlist = foldl (\s m -> m^.mPostId : s) []

postids :: Show a => Seq.Seq (a, Message) -> String
postids nms = intersperse ", " $ fmap pid nms
    where pid (n, m) = show n <> ".mPostID=" <> show (m^.mPostId)
          intersperse b ls = case Seq.viewl ls of
                               Seq.EmptyL -> ""
                               l Seq.:< r -> l <> b <> (intersperse b r)

uniqueIds :: Foldable t => t Message -> Bool
uniqueIds msgs =
    let ids = idlist msgs
    in  length ids == length (U.unique ids)

validIds :: Foldable t => t Message -> Bool
validIds = null . filter isNothing . idlist

tastyBatch :: TestBatch -> TestTree
tastyBatch b = testGroup (fst b) $ tastyTests (snd b)
    where tastyTests = map tastyTest
          tastyTest = uncurry testProperty

tests :: TestTree
tests = testGroup "Messages Tests"
        [ createTests
        , movementTests
        , reversalTests
        , splitTests
        , instanceTests
        ]


createTests :: TestTree
createTests = testGroup "Create"
              [ testCase "no messages"
                    $ 0 @=? countMessages noMessages
              , testProperty "has messages"
                    $ \x -> not (emptyMessages x) ==> 0 /= countMessages x
              , testProperty "append to empty"
                    $ \x -> 1 == (countMessages $ appendMessage x noMessages)
              , testProperty "append to append to empty"
                    $ \(x, y) -> 2 == (countMessages $
                                       appendMessage x $
                                       appendMessage y noMessages)
              , testProperty "join to empty"
                    $ \(x, y) ->
                        let m1 = appendMessage x $ appendMessage y noMessages
                            m2 = noMessages
                        in (2 == (countMessages $ m1 <> m2) &&
                            2 == (countMessages $ m2 <> m1))
              , testProperty "join to one"
                    $ \(x, y, z) ->
                        let m1 = appendMessage x $ appendMessage y noMessages
                            m2 = appendMessage z noMessages
                            j1 = m1 <> m2
                            j2 = m2 <> m1
                        in (3 == (countMessages $ j1) &&
                            3 == (countMessages $ j2) &&
                            idlist [y, x, z] == idlist j1 &&
                            idlist [z, y, x] == idlist j2)

              , testProperty "join to many"
                    $ \(w, x, y, z) ->
                        let m1 = appendMessage x $ appendMessage y noMessages
                            m2 = appendMessage w $ appendMessage z noMessages
                            j1 = m1 <> m2
                            j2 = m2 <> m1
                        in (4 == (countMessages $ j1) &&
                            4 == (countMessages $ j2) &&
                            idlist [y, x, z, w] == idlist j1 &&
                            idlist [z, w, y, x] == idlist j2)

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

moveDownTestMultipleStart :: TestTree
moveDownTestMultipleStart =
    testProperty "Move down in multiple messages from the start" $
                     \(x, y, z) ->
                         let msgs = makeMsgs [ postMsg x, postMsg y, postMsg z]
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
                         let msgs = makeMsgs [ postMsg x, postMsg y, postMsg z]
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
                         let msgs = makeMsgs [ postMsg x, postMsg y, postMsg z]
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
                         let msgs = makeMsgs [ postMsg x, postMsg y, postMsg z]
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
                         let msgs = makeMsgs [ postMsg w
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
                         let msgs = makeMsgs [ postMsg w
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
                         let msgs = makeMsgs [ delMsg w
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
                         let msgs = makeMsgs [ delMsg w
                                             , delMsg x
                                             , delMsg y
                                             , delMsg z]
                             msgid = getPrevPostId ((delMsg z)^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

reversalTests :: TestTree
reversalTests = testGroup "Reversal"
                [ testProperty "round trip" $
                \l -> let rr = unreverseMessages (reverseMessages l)
                          ids = idlist l
                          rr_ids = idlist rr
                      in getLastPostId l == getLastPostId rr && ids == rr_ids
                , testCase "reverse nothing" $
                  (emptyMessages $ reverseMessages noMessages) @?
                  "reverse of empty Messages"
                ]

splitTests :: TestTree
splitTests = testGroup "Split"
             [ testCase "split nothing on empty" $
                        let (m, _) = splitMessages Nothing noMessages
                        in isNothing m @? "must be nothing"
             , testProperty "split just on empty" $ \x ->
                   let (m, _) = splitMessages (Just x) noMessages
                   in isNothing m
             , testProperty "split nothing on list" $ \x ->
                 let (m, _) = splitMessages Nothing x
                 in isNothing m
             , testProperty "split nothing on not found" $ \(w, x, y, z) ->
                 let (m, _) = splitMessages (w^.mPostId) msgs
                     msgs = foldr appendMessage noMessages [x, y, z]
                     idents = postids $ Seq.zip (Seq.fromList "wxyz") msgs
                     info = idents <> " against " <> show ((fromJust m)^.mPostId)
                 in uniqueIds [w, x, y, z] ==>
                    counterexample info $ isNothing m
             , testProperty "all before reversed on split nothing"
                   $ \(w, x, y, z) ->
                       let (_, (before, _)) = splitMessages Nothing msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           control = idlist (reverse inpl)
                           result = idlist before
                           info = show control <> " /= " <> show result
                       in counterexample info $ control == result
             , testProperty "all before reversed on not found"
                   $ \(w, x, y, z) ->
                       let (_, (before, _)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [x, y, z]
                       in uniqueIds [w, x, y, z] ==>
                          idlist (reverse inpl) == idlist before

             , testProperty "found at first position"
                   $ \(w, x, y, z) ->
                       let (m, _) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                       in validIds inpl && uniqueIds inpl ==>
                          w^.mPostId == (fromJust m)^.mPostId
             , testProperty "no before when found at first position"
                   $ \(w, x, y, z) ->
                       let (_, (before, _)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $ emptyMessages before
             , testProperty "remaining after when found at first position"
                   $ \(w, x, y, z) ->
                       let (_, (_, after)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (tail inpl) == idlist after

             , testProperty "found at last position"
                   $ \(w, x, y, z) ->
                       let (m, _) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                       in validIds inpl && uniqueIds inpl ==>
                          z^.mPostId == (fromJust m)^.mPostId
             , testProperty "reversed before when found at last position"
                   $ \(w, x, y, z) ->
                       let (_, (before, _)) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (reverse $ init inpl) == idlist before
             , testProperty "no after when found at last position"
                   $ \(w, x, y, z) ->
                       let (_, (_, after)) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $ emptyMessages after

             , testProperty "found at midpoint position"
                   $ \(v, w, x, y, z) ->
                       let (m, _) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                       in validIds inpl && uniqueIds inpl ==>
                          x^.mPostId == (fromJust m)^.mPostId
             , testProperty "reversed before when found at midpoint position"
                   $ \(v, w, x, y, z) ->
                       let (_, (before, _)) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (reverse $ take 2 inpl) == idlist before
             , testProperty "after when found at midpoint position"
                   $ \(v, w, x, y, z) ->
                       let (_, (_, after)) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (drop 3 inpl) == idlist after
             ]


instanceTests :: TestTree
instanceTests = testGroup "Messages Instances"
                [ tastyBatch (monoid (undefined :: Messages))
                ]

instance EqProp Messages where
    a =-= b = idlist a =-= idlist b

instance EqProp PostId where
    a =-= b = (show $ idString a) =-= (show $ idString b)
