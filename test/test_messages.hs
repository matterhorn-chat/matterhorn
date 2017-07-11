{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Exception
import           Data.List (intercalate, sortBy)
import qualified Data.List.UniqueUnsorted as U
import qualified Data.Map as Map
import           Data.Maybe (isNothing, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), getCurrentTime
                                 , secondsToDiffTime)
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
import           Types.Posts

main :: IO ()
main = defaultMain tests `catch` (\e -> do
                                    if e == ExitSuccess
                                    then putStrLn "Passed"
                                    else do putStrLn "FAILED"
                                            throwIO e)

tests :: TestTree
tests = testGroup "Messages Tests"
        [ createTests
        , movementTests
        , reversalTests
        , splitTests
        , instanceTests
        ]


test_m1 :: IO Message
test_m1 = do t1 <- getCurrentTime
             return $ Message Seq.empty Nothing t1 (CP NormalPost) False False Seq.empty NotAReply Nothing Map.empty Nothing False Nothing

test_m2 :: IO Message
test_m2 = do t2 <- getCurrentTime
             return $ Message Seq.empty Nothing t2 (CP Emote) False False Seq.empty NotAReply (Just $ fromId $ Id $ T.pack "m2") Map.empty Nothing False Nothing

test_m3 :: IO Message
test_m3 = do t3 <- getCurrentTime
             return $ Message Seq.empty Nothing t3 (CP NormalPost) False False Seq.empty NotAReply (Just $ fromId $ Id $ T.pack "m3") Map.empty Nothing False Nothing

setDateOrderMessages :: [Message] -> [Message]
setDateOrderMessages = snd . foldl setTimeAndInsert (startTime, [])
    where setTimeAndInsert (t, ml) m = let t2 = tick t
                                       in (t2, ml ++ [m {_mDate = t2}])
          startTime = UTCTime (ModifiedJulianDay 100) (secondsToDiffTime 0)
          tick (UTCTime d t) = UTCTime d $ succ t

makeMsgs :: [Message] -> Messages
makeMsgs = foldr addMessage noMessages

idlist :: Foldable t => t Message -> [Maybe PostId]
idlist = foldr (\m s -> m^.mPostId : s) []

postids :: (Foldable t) => String -> t Message -> String
postids names msgs = let zipf = (\(n,z) m -> if null n
                                             then ("", ('?', m) : z)
                                             else (init n, (last n, m) : z))
                         zipped = snd $ foldr (flip zipf) (names, []) msgs
                         pid (n, m) = show n <> ".mPostID=" <> show (m^.mPostId)
                     in intercalate ", " $ map pid zipped

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

createTests :: TestTree
createTests = testGroup "Create"
              [ testCase "no messages"
                    $ 0 @=? length noMessages
              , testProperty "has messages"
                    $ \x -> not (null (x :: Messages)) ==> 0 /= length x
              , testProperty "add to empty"
                    $ \x -> 1 == (length $ addMessage x noMessages)
              , testProperty "add to add to empty"
                    $ \(x, y) -> 2 == (length $ makeMsgs [x, y])
              , testProperty "join to empty"
                    $ \(x, y) ->
                        let m1 = makeMsgs [x, y]
                            m2 = noMessages
                        in (2 == (length $ m1 <> m2) &&
                            2 == (length $ m2 <> m1))

              , testProperty "join one to many"
                    $ \(x, y, z) ->
                        let l1 = setDateOrderMessages [x, y]
                            m1 = makeMsgs l1
                            m2 = addMessage z noMessages
                            j2 = m2 <> m1
                        in idlist [z, x, y] === idlist j2

              , testProperty "join many to one"
                    $ \(x, y, z) ->
                        let l1 = setDateOrderMessages [x, y]
                            m1 = makeMsgs l1
                            m2 = addMessage z noMessages
                            j1 = m1 <> m2
                        in idlist [x, y, z] === idlist j1

              , testProperty "join to many"
                    $ \(w, x, y, z) ->
                        let l1 = setDateOrderMessages [x, y]
                            l2 = setDateOrderMessages [w, z]
                            m1 = makeMsgs l1
                            m2 = makeMsgs l2
                            -- note that mappend is literal: there is
                            -- no date relationship between the
                            -- members l1 and l2 and mappend doesn't
                            -- enforce one.
                            j1 = m1 <> m2
                            j2 = m2 <> m1
                        in (4 == (length j1) &&
                            4 == (length j2) &&
                            idlist (l1 <> l2) == idlist j1 &&
                            idlist (l2 <> l1) == idlist j2)

              , testProperty "natural ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                        in idlist l === idlist (makeMsgs l)

              , testProperty "reverse ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                        in idlist l === idlist (makeMsgs $ reverse l)

              , testProperty "mirrored ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                            [w', x', y', z'] = l
                        in idlist l === idlist (makeMsgs [y', z', w', x'])

              , testProperty "ordering 1 of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                            [w', x', y', z'] = l
                        in
                           idlist l === idlist (makeMsgs [y', w', z', x'])

              , testProperty "ordering 2 of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                            [w', x', y', z'] = l
                        in idlist l === idlist (makeMsgs [x', z', w', y'])

              , testProperty "duplicated last addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ map postMsg [w, x, y, z]
                        in uniqueIds l ==>
                          idlist l === idlist (makeMsgs $ [last l] <> l)

              , testProperty "duplicated natural ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ map postMsg [w, x, y, z]
                        in idlist l === idlist (makeMsgs $ l <> l)

              , testProperty "duplicated reverse ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ map postMsg [w, x, y, z]
                        in idlist l === idlist (makeMsgs $ reverse l <> l)

              , testProperty "duplicated mirrored ordering of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ map postMsg [w, x, y, z]
                            [w', x', y', z'] = l
                        in idlist l === idlist (makeMsgs $ [y', z', w', x'] <> l)

              , testProperty "duplicated ordering 1 of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ postMsg <$> [w, x, y, z]
                            [w', x', y', z'] = l
                        in idlist l === idlist (makeMsgs $ [y', w', z', x'] <> l)

              , testProperty "duplicated ordering 2 of addMessage"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages $ postMsg <$> [w, x, y, z]
                            [w', x', y', z'] = l
                        in idlist l === idlist (makeMsgs $ [x', z', w', y'] <> l)

              , testProperty "non-posted are not duplicate removed"
                    $ \(w, x, y, z) ->
                        let l = setDateOrderMessages [w, x, y, z]
                            [w', x', y', z'] = l
                            l' = [x', z', w', y']
                            ex = sortBy (\a b -> compare (a^.mDate) (b^.mDate))
                                 ([e | e <- l', isNothing (e^.mPostId) ] <> l)
                        in idlist ex === idlist (makeMsgs $ l' <> l)

              , testProperty "duplicate dates different IDs in posted order"
                    $ \(w, x, y, z) ->
                        let d = UTCTime
                                (ModifiedJulianDay 1234)
                                (secondsToDiffTime 9876)
                            l = foldl (setTime d) [] $ postMsg <$> [w, x, y, z]
                            setTime t ml m = ml ++ [m {_mDate = t}]
                            [w', x', y', z'] = l
                            l' = [x', z', w', y']
                            ex = l
                        in uniqueIds l ==>
                           idlist ex === idlist (makeMsgs $ l' <> l)


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
                   \x -> let msgs = addMessage x noMessages
                         in Nothing == (getNextPostId (x^.mPostId) msgs)

moveUpTestSingle :: TestTree
moveUpTestSingle = testProperty "Move down from single message" $
                    \x -> let msgs = addMessage x noMessages
                          in Nothing == (getPrevPostId (x^.mPostId) msgs)

moveDownTestMultipleStart :: TestTree
moveDownTestMultipleStart =
    testProperty "Move down in multiple messages from the start" $
                     \(x', y', z') ->
                         let [x, y, z] = setDateOrderMessages
                                         [ postMsg x'
                                         , postMsg y'
                                         , postMsg z'
                                         ]
                             msgs = makeMsgs [x, y, z]
                             msgid = getNextPostId (x^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "xyz" msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $
                                y^.mPostId == msgid

moveUpTestMultipleStart :: TestTree
moveUpTestMultipleStart =
    testProperty "Move up in multiple messages from the start" $
                     \(x', y', z') ->
                         let [x, y, z] = setDateOrderMessages
                                         [ postMsg x', postMsg y', postMsg z']
                             msgs = makeMsgs [x, y, z]
                             msgid = getPrevPostId (x^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "xyz" msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

moveDownTestMultipleEnd :: TestTree
moveDownTestMultipleEnd =
    testProperty "Move down in multiple messages from the end" $
                     \(x', y', z') ->
                         let [x, y, z] = setDateOrderMessages
                                         [ postMsg x', postMsg y', postMsg z']
                             msgs = makeMsgs [x, y, z]
                             msgid = getNextPostId (z^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "xyz" msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

moveUpTestMultipleEnd :: TestTree
moveUpTestMultipleEnd =
    testProperty "Move up in multiple messages from the end" $
                     \(x', y', z') ->
                         let [x, y, z] = setDateOrderMessages
                                         [ postMsg x', postMsg y', postMsg z']
                             msgs = makeMsgs [x, y, z]
                             msgid = getPrevPostId (z^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "xyz" msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ (y^.mPostId) == msgid

moveDownTestMultipleSkipDeleted :: TestTree
moveDownTestMultipleSkipDeleted =
    testProperty "Move down in multiple messages skipping deleteds" $
                     \(w', x', y', z') ->
                         let [w, x, y, z] = setDateOrderMessages
                                            [ postMsg w'
                                            , delMsg x'
                                            , delMsg y'
                                            , postMsg z']
                             msgs = makeMsgs [w, x, y, z]
                             msgid = getNextPostId (w^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "wxyz" msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ (z^.mPostId) == msgid

moveUpTestMultipleSkipDeleted :: TestTree
moveUpTestMultipleSkipDeleted =
    testProperty "Move one up in multiple messages skipping deleteds" $
                     \(w', x', y', z') ->
                         let [w, x, y, z] = setDateOrderMessages
                                            [ postMsg w'
                                            , delMsg x'
                                            , delMsg y'
                                            , postMsg z']
                             msgs = makeMsgs [w, x, y, z]
                             msgid = getPrevPostId (z^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "wxyz" msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ (w^.mPostId) == msgid

moveDownTestMultipleSkipDeletedAll :: TestTree
moveDownTestMultipleSkipDeletedAll =
    testProperty "Move one down in multiple deleted messages skipping deleteds" $
                     \(w', x', y', z') ->
                         -- n.b. current selected is also deleted,
                         -- which can happen due to multi-user async
                         -- server changes.
                         let [w, x, y, z] = setDateOrderMessages
                                            [ delMsg w'
                                            , delMsg x'
                                            , delMsg y'
                                            , delMsg z']
                             msgs = makeMsgs [w, x, y, z]
                             msgid = getNextPostId (w^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "wxyz" msgs
                             info = idents <> " against " <> show msgid
                         in counterexample info $ Nothing == msgid

moveUpTestMultipleSkipDeletedAll :: TestTree
moveUpTestMultipleSkipDeletedAll =
    testProperty "Move one up in multiple deleted messages skipping deleteds" $
                     \(w', x', y', z') ->
                         -- n.b. current selected is also deleted,
                         -- which can happen due to multi-user async
                         -- server changes.
                         let [w, x, y, z] = setDateOrderMessages
                                            [ delMsg w'
                                            , delMsg x'
                                            , delMsg y'
                                            , delMsg z']
                             msgs = makeMsgs [w, x, y, z]
                             msgid = getPrevPostId (z^.mPostId) msgs
                             -- for useful info on failure:
                             idents = postids "wxyz" msgs
                             info = idents <> " against " <> show msgid
                         in uniqueIds msgs ==>
                            counterexample info $ Nothing == msgid

reversalTests :: TestTree
reversalTests = testGroup "Reversal"
                [ testProperty "round trip" $
                     \l -> let rr = unreverseMessages (reverseMessages l)
                           in idlist l === idlist rr
                , testProperty "getLatestMessage finds same in either dir" $
                     \l -> let rr = unreverseMessages (reverseMessages l)  -- KWQ: just one reverse, not two
                           in getLatestPostId l === getLatestPostId rr
                , testCase "reverse nothing" $
                      (null $ unreverseMessages $ reverseMessages noMessages) @?
                      "reverse of empty Messages"
                , testProperty "reverse order" $
                      \l -> let r = reverseMessages l
                            in idlist l === reverse (idlist r)
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

             , testProperty "split nothing on not found" $ \(w', x', y', z') ->
                 let (m, _) = splitMessages (w^.mPostId) msgs
                     [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                     msgs = makeMsgs [x, y, z]
                     idents = postids "wxyz" msgs
                     info = idents <> " against " <> show ((fromJust m)^.mPostId)
                 in uniqueIds [w, x, y, z] ==>
                    counterexample info $ isNothing m

             , testProperty "all before reversed on split nothing"
                   $ \(w, x, y, z) ->
                       let (_, (before, _)) = splitMessages Nothing msgs
                           msgs = makeMsgs inpl
                           inpl = setDateOrderMessages [w, x, y, z]
                           control = idlist (reverse inpl)
                           result = idlist before
                           info = show control <> " /= " <> show result
                       in counterexample info $ control == result

             , testProperty "all before reversed on not found"
                   $ \(w', x', y', z') ->
                       let (_, (before, _)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                       in uniqueIds [w, x, y, z] ==>
                          idlist (reverse inpl) == idlist before

             , testProperty "found at first position"
                   $ \(w', x', y', z') ->
                       let (m, _) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                       in validIds inpl && uniqueIds inpl ==>
                          w^.mPostId == (fromJust m)^.mPostId

             , testProperty "no before when found at first position"
                   $ \(w', x', y', z') ->
                       let (_, (before, _)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $ null $ unreverseMessages before
             , testProperty "remaining after when found at first position"
                   $ \(w', x', y', z') ->
                       let (_, (_, after)) = splitMessages (w^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (tail inpl) == idlist after

             , testProperty "found at last position"
                   $ \(w', x', y', z') ->
                       let (m, _) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                       in validIds inpl && uniqueIds inpl ==>
                          z^.mPostId == (fromJust m)^.mPostId

             , testProperty "reversed before when found at last position"
                   $ \(w', x', y', z') ->
                       let (_, (before, _)) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist (reverse $ init inpl) == idlist before

             , testProperty "no after when found at last position"
                   $ \(w', x', y', z') ->
                       let (_, (_, after)) = splitMessages (z^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [w, x, y, z]
                           [w, x, y, z] = setDateOrderMessages [w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $ null after

             , testProperty "found at midpoint position"
                   $ \(v', w', x', y', z') ->
                       let (m, _) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                           [v, w, x, y, z] = setDateOrderMessages
                                             [v', w', x', y', z']
                       in validIds inpl && uniqueIds inpl ==>
                          x^.mPostId == (fromJust m)^.mPostId

             , testProperty "reversed before when found at midpoint position"
                   $ \(v', w', x', y', z') ->
                       let (_, (before, _)) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                           [v, w, x, y, z] = setDateOrderMessages
                                             [v', w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist before)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist [w, v] == idlist before

             , testProperty "after when found at midpoint position"
                   $ \(v', w', x', y', z') ->
                       let (_, (_, after)) = splitMessages (x^.mPostId) msgs
                           msgs = makeMsgs inpl
                           inpl = [v, w, x, y, z]
                           [v, w, x, y, z] = setDateOrderMessages
                                             [v', w', x', y', z']
                           info = show (idlist inpl) <> " ==> " <> (show $ idlist after)
                       in validIds inpl && uniqueIds inpl ==>
                          counterexample info $
                                         idlist [y, z] == idlist after
             ]


instanceTests :: TestTree
instanceTests = testGroup "Messages Instances"
                $ map tastyBatch
                      [ (monoid (undefined :: Messages))
                      , (monoid (undefined :: RetrogradeMessages))
                      ]

instance EqProp Messages where
    a =-= b = idlist a =-= idlist b

instance EqProp RetrogradeMessages where
    a =-= b = idlist a =-= idlist b

instance EqProp PostId where
    a =-= b = (show $ idString a) =-= (show $ idString b)
