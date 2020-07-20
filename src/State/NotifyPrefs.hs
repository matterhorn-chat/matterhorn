{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module State.NotifyPrefs
    ( enterEditNotifyPrefsMode
    )
where

import Prelude ()
import Prelude.MH

import Types
import Types.Channels ( channelNotifyPropsMarkUnreadL
                      , channelNotifyPropsIgnoreChannelMentionsL
                      , channelNotifyPropsDesktopL
                      , channelNotifyPropsPushL
                      )

import Network.Mattermost.Types ( ChannelNotifyProps
                                , channelNotifyPropsMarkUnread
                                , channelNotifyPropsIgnoreChannelMentions
                                , WithDefault(..)
                                , NotifyOption(..)
                                )

import Brick
import Brick.Forms
import Lens.Micro.Platform ( Lens', (.=), lens )

muteLens :: Lens' ChannelNotifyProps Bool
muteLens = lens (\props -> props^.channelNotifyPropsMarkUnreadL == IsValue NotifyOptionMention)
           (\props muted -> props { channelNotifyPropsMarkUnread =
                                          if muted
                                          then IsValue NotifyOptionMention
                                          else IsValue NotifyOptionAll
                                  })

channelMentionLens :: Lens' ChannelNotifyProps Bool
channelMentionLens = lens (\props -> (props^.channelNotifyPropsIgnoreChannelMentionsL) == (IsValue True))
                     (\props ignoreChannelMentions ->
                          props { channelNotifyPropsIgnoreChannelMentions = if ignoreChannelMentions
                                                                            then IsValue True
                                                                            else Default
                                })

mkNotifyButtons :: ((WithDefault NotifyOption) -> Name)
                -> Lens' ChannelNotifyProps (WithDefault NotifyOption)
                -> ChannelNotifyProps
                -> FormFieldState ChannelNotifyProps e Name
mkNotifyButtons mkName l =
    radioField l [ (Default, mkName Default, "Global default")
                 , (IsValue NotifyOptionAll, mkName $ IsValue NotifyOptionAll, "All activity")
                 , (IsValue NotifyOptionMention, mkName $ IsValue NotifyOptionMention, "Mentions")
                 , (IsValue NotifyOptionNone, mkName $ IsValue NotifyOptionNone, "Never")
                 ]
    
notifyPrefsForm :: ChannelNotifyProps -> Form ChannelNotifyProps e Name
notifyPrefsForm =
    newForm [ checkboxField muteLens MuteToggleField "Mute channel"
            , checkboxField channelMentionLens ChannelMentionsField "Ignore channel mentions"
            , (str "Desktop notifications" <=>) . (padLeft $ Pad 1) @@=
                mkNotifyButtons DesktopNotificationsField channelNotifyPropsDesktopL
            , (str "Push notifications" <=>) . (padLeft $ Pad 1) @@=
                mkNotifyButtons PushNotificationsField channelNotifyPropsPushL
            ]
