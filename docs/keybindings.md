
# Keybindings: Help Page
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Return to the main interface |
| `PgDown` | `page-down` | Page down |
| `PgUp` | `page-up` | Page up |
| `End` | `scroll-bottom` | Scroll to the end of the help |
| `Down` | `scroll-down` | Scroll down |
| `Home` | `scroll-top` | Scroll to the beginning of the help |
| `Up` | `scroll-up` | Scroll up |

# Keybindings: Main Interface
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel autocomplete, message reply, or edit, in that order |
| `M-l` | `clear-unread` | Clear the current channel's unread / edited indicators |
| `C-g` | `enter-fast-select` | Enter fast channel selection mode |
| `C-o` | `enter-url-open` | Select and open a URL posted to the current channel |
| `M-s` | `focus-last-channel` | Change to the most recently-focused channel |
| `C-n` | `focus-next-channel` | Change to the next channel in the channel list |
| `M-a` | `focus-next-unread` | Change to the next channel with unread messages |
| `C-p` | `focus-prev-channel` | Change to the previous channel in the channel list |
| `M-k` | `invoke-editor` | Invoke *$EDITOR* to edit the current message |
| `PgUp` | `page-up` | Page up in the channel message list (enters message select mode) |
| `C-q` | `quit` | Quit |
| `C-r` | `reply-recent` | Reply to the most recent message |
| `Down` | `scroll-down` | Scroll down in the channel input history |
| `Home` | `scroll-top` | Scroll to top of channel message list |
| `Up` | `scroll-up` | Scroll up in the channel input history |
| `C-s` | `select-mode` | Select a message to edit/reply/delete |
| `C-x` | `show-attachment-list` | Show the attachment list |
| `M-8` | `show-flagged-posts` | View currently flagged posts |
| `F1` | `show-help` | Show this help screen |
| `F2` | `toggle-channel-list-visibility` | Toggle channel list visibility |
| `M-p` | `toggle-message-preview` | Toggle message preview |
| `M-e` | `toggle-multiline` | Toggle multi-line message compose mode |

# Keybindings: Channel Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel channel selection |
| `C-n` | `focus-next-channel` | Select next match |
| `C-p` | `focus-prev-channel` | Select previous match |

# Keybindings: URL Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel URL selection |
| `j`, `Down` | `select-down` | Move cursor down |
| `k`, `Up` | `select-up` | Move cursor up |

# Keybindings: Message Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel message selection |
| `d` | `delete-message` | Delete the selected message (with confirmation) |
| `e` | `edit-message` | Begin editing the selected message |
| `Enter` | `fetch-for-gap` | Fetch messages for the selected gap |
| `f` | `flag-message` | Flag the selected message |
| `o` | `open-message-url` | Open all URLs in the selected message |
| `PgDown` | `page-down` | Move the cursor down by 10 messages |
| `PgUp` | `page-up` | Move the cursor up by 10 messages |
| `a` | `react-to-message` | Post a reaction to the selected message |
| `r` | `reply-message` | Begin composing a reply to the selected message |
| `End` | `scroll-bottom` | Scroll to bottom and select the latest message |
| `Home` | `scroll-top` | Scroll to top and select the oldest message |
| `j`, `Down` | `select-down` | Select the next message |
| `k`, `Up` | `select-up` | Select the previous message |
| `v` | `view-message` | View the selected message |
| `y` | `yank-message` | Copy a verbatim section or message to the clipboard |
| `Y` | `yank-whole-message` | Copy an entire message to the clipboard |

# Keybindings: Flagged Messages
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | `activate-list-item` | Jump to and select current message |
| `Esc`, `C-c` | `cancel` | Exit post browsing |
| `f` | `flag-message` | Toggle the selected message flag |
| `j`, `Down` | `select-down` | Select the next message |
| `k`, `Up` | `select-up` | Select the previous message |

# Keybindings: User Listings
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | `activate-list-item` | Interact with the selected user |
| `Esc`, `C-c` | `cancel` | Close the user search list |
| `PgDown` | `page-down` | Page down in the user list |
| `PgUp` | `page-up` | Page up in the user list |
| `C-n`, `Down` | `search-select-down` | Select the next user |
| `C-p`, `Up` | `search-select-up` | Select the previous user |

# Keybindings: Channel Search Window
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | `activate-list-item` | Join the selected channel |
| `Esc`, `C-c` | `cancel` | Close the channel search list |
| `PgDown` | `page-down` | Page down in the channel list |
| `PgUp` | `page-up` | Page up in the channel list |
| `C-n`, `Down` | `search-select-down` | Select the next channel |
| `C-p`, `Up` | `search-select-up` | Select the previous channel |

# Keybindings: Reaction Emoji Search Window
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | `activate-list-item` | Post the selected emoji reaction |
| `Esc`, `C-c` | `cancel` | Close the emoji search window |
| `PgDown` | `page-down` | Page down in the emoji list |
| `PgUp` | `page-up` | Page up in the emoji list |
| `C-n`, `Down` | `search-select-down` | Select the next emoji |
| `C-p`, `Up` | `search-select-up` | Select the previous emoji |

# Keybindings: Message Viewer
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close window |
| `PgDown` | `page-down` | Page down |
| `PgUp` | `page-up` | Page up |
| `End` | `scroll-bottom` | Scroll to the end of the message |
| `Down` | `scroll-down` | Scroll down |
| `Home` | `scroll-top` | Scroll to the beginning of the message |
| `Up` | `scroll-up` | Scroll up |

# Keybindings: Attachment List
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `a` | `add-to-attachment-list` | Add a new attachment to the attachment list |
| `Esc`, `C-c` | `cancel` | Close attachment list |
| `d` | `delete-from-attachment-list` | Delete the selected attachment from the attachment list |
| `o` | `open-attachment` | Open the selected attachment using the URL open command |
| `j`, `Down` | `select-down` | Move cursor down |
| `k`, `Up` | `select-up` | Move cursor up |

