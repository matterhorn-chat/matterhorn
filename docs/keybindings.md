# Keybindings

# Global Keybindings
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `F1` | `show-help` | Show this help screen |

# Help Page
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Up` | `scroll-up` | Scroll up |
| `Down` | `scroll-down` | Scroll down |
| `PgUp` | `page-up` | Page up |
| `PgDown` | `page-down` | Page down |
| `Esc`, `C-c` | `cancel` | Return to the previous interface |
| `End` | `scroll-bottom` | Scroll to the end of the help |
| `Home` | `scroll-top` | Scroll to the beginning of the help |

# Main Interface
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `C-s` | `select-mode` | Select a message to edit/reply/delete |
| `C-r` | `reply-recent` | Reply to the most recent message |
| `M-p` | `toggle-message-preview` | Toggle message preview |
| `F2` | `toggle-channel-list-visibility` | Toggle channel list visibility |
| `F3` | `toggle-expanded-channel-topics` | Toggle display of expanded channel topics |
| `C-Right` | `next-team` | Switch to the next available team |
| `C-Left` | `prev-team` | Switch to the previous available team |
| (unbound) | `move-current-team-left` | Move the current team to the left in the team list |
| (unbound) | `move-current-team-right` | Move the current team to the right in the team list |
| `M-k` | `invoke-editor` | Invoke `$EDITOR` to edit the current message |
| `C-g` | `enter-fast-select` | Enter fast channel selection mode |
| `C-q` | `quit` | Quit |
| `Tab` | (non-customizable key) | Tab-complete forward |
| `BackTab` | (non-customizable key) | Tab-complete backward |
| `Up` | `scroll-up` | Scroll up in the channel input history |
| `Down` | `scroll-down` | Scroll down in the channel input history |
| `PgUp` | `page-up` | Page up in the channel message list (enters message select mode) |
| `S-Home` | `select-oldest-message` | Scroll to top of channel message list |
| `C-n` | `focus-next-channel` | Change to the next channel in the channel list |
| `C-p` | `focus-prev-channel` | Change to the previous channel in the channel list |
| `M-a` | `focus-next-unread` | Change to the next channel with unread messages or return to the channel marked '~' |
| `C-x` | `show-attachment-list` | Show the attachment list |
| (unbound) | `focus-next-unread-user-or-channel` | Change to the next channel with unread messages preferring direct messages |
| `M-s` | `focus-last-channel` | Change to the most recently-focused channel |
| `Enter` | (non-customizable key) | Send the current message |
| `C-o` | `enter-url-open` | Select and open a URL posted to the current channel |
| `M-l` | `clear-unread` | Clear the current channel's unread / edited indicators |
| `M-e` | `toggle-multiline` | Toggle multi-line message compose mode |
| `Esc`, `C-c` | `cancel` | Cancel autocomplete, message reply, or edit, in that order |
| `M-8` | `show-flagged-posts` | View currently flagged posts |

# Text Editing
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `C-t` | `editor-transpose-chars` | Transpose the final two characters |
| `C-a` | `editor-beginning-of-line` | Go to the start of the current line |
| `C-e` | `editor-end-of-line` | Go to the end of the current line |
| `C-d` | `editor-delete-char` | Delete the character at the cursor |
| `C-u` | `editor-kill-to-beginning-of-line` | Delete from the cursor to the start of the current line |
| `C-k` | `editor-kill-to-end-of-line` | Kill the line to the right of the current position and copy it |
| `C-f` | `editor-next-char` | Move one character to the right |
| `C-b` | `editor-prev-char` | Move one character to the left |
| `M-f` | `editor-next-word` | Move one word to the right |
| `M-b` | `editor-prev-word` | Move one word to the left |
| `C-w`, `M-Backspace` | `editor-delete-prev-word` | Delete the word to the left of the cursor |
| `M-d` | `editor-delete-next-word` | Delete the word to the right of the cursor |
| `Home` | `editor-home` | Move the cursor to the beginning of the input |
| `End` | `editor-end` | Move the cursor to the end of the input |
| `C-y` | `editor-yank` | Paste the current buffer contents at the cursor |

# Channel Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | (non-customizable key) | Switch to selected channel |
| `Esc`, `C-c` | `cancel` | Cancel channel selection |
| `C-n` | `focus-next-channel` | Select next match |
| `C-p` | `focus-prev-channel` | Select previous match |
| `Down` | `focus-next-channel-alternate` | Select next match (alternate binding) |
| `Up` | `focus-prev-channel-alternate` | Select previous match (alternate binding) |

# Message Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel message selection |
| `k`, `Up` | `select-up` | Select the previous message |
| `j`, `Down` | `select-down` | Select the next message |
| `Home` | `scroll-top` | Scroll to top and select the oldest message |
| `End` | `scroll-bottom` | Scroll to bottom and select the latest message |
| `PgUp` | `page-up` | Move the cursor up by 10 messages |
| `PgDown` | `page-down` | Move the cursor down by 10 messages |
| `o` | `open-message-url` | Open all URLs in the selected message |
| `r` | `reply-message` | Begin composing a reply to the selected message |
| `e` | `edit-message` | Begin editing the selected message |
| `d` | `delete-message` | Delete the selected message (with confirmation) |
| `y` | `yank-message` | Copy a verbatim section or message to the clipboard |
| `Y` | `yank-whole-message` | Copy an entire message to the clipboard |
| `p` | `pin-message` | Toggle whether the selected message is pinned |
| `f` | `flag-message` | Flag the selected message |
| `v` | `view-message` | View the selected message |
| `Enter` | `fetch-for-gap` | Fetch messages for the selected gap |
| `a` | `react-to-message` | Post a reaction to the selected message |

# User Listings
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close the user search list |
| `C-p`, `Up` | `search-select-up` | Select the previous user |
| `C-n`, `Down` | `search-select-down` | Select the next user |
| `PgDown` | `page-down` | Page down in the user list |
| `PgUp` | `page-up` | Page up in the user list |
| `Enter` | `activate-list-item` | Interact with the selected user |

# URL Select Mode
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Enter` | (non-customizable key) | Open the selected URL, if any |
| `s` | `save-attachment` | Save the selected attachment |
| `Esc`, `C-c` | `cancel` | Cancel URL selection |
| `k`, `Up` | `select-up` | Move cursor up |
| `j`, `Down` | `select-down` | Move cursor down |
| `q` | (non-customizable key) | Cancel URL selection |

# Theme List Window
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close the theme list |
| `C-p`, `Up` | `search-select-up` | Select the previous theme |
| `C-n`, `Down` | `search-select-down` | Select the next theme |
| `PgDown` | `page-down` | Page down in the theme list |
| `PgUp` | `page-up` | Page up in the theme list |
| `Enter` | `activate-list-item` | Switch to the selected color theme |

# Channel Search Window
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close the channel search list |
| `C-p`, `Up` | `search-select-up` | Select the previous channel |
| `C-n`, `Down` | `search-select-down` | Select the next channel |
| `PgDown` | `page-down` | Page down in the channel list |
| `PgUp` | `page-up` | Page up in the channel list |
| `Enter` | `activate-list-item` | Join the selected channel |

# Message Viewer: Common
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close window |
| `Tab` | `select-next-tab` | Select next tab |
| `BackTab` | `select-previous-tab` | Select previous tab |

# Message Viewer: Message tab
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `PgUp` | `page-up` | Page up |
| `PgDown` | `page-down` | Page down |
| `S-Left` | `page-left` | Page left |
| `S-Right` | `page-right` | Page right |
| `Up` | `scroll-up` | Scroll up |
| `Down` | `scroll-down` | Scroll down |
| `Left` | `scroll-left` | Scroll left |
| `Right` | `scroll-right` | Scroll right |
| `End` | `scroll-bottom` | Scroll to the end of the message |
| `Home` | `scroll-top` | Scroll to the beginning of the message |

# Message Viewer: Reactions tab
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `PgUp` | `page-up` | Page up |
| `PgDown` | `page-down` | Page down |
| `Up` | `scroll-up` | Scroll up |
| `Down` | `scroll-down` | Scroll down |
| `End` | `scroll-bottom` | Scroll to the end of the reactions list |
| `Home` | `scroll-top` | Scroll to the beginning of the reactions list |

# Attachment List
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close attachment list |
| `k`, `Up` | `select-up` | Move cursor up |
| `j`, `Down` | `select-down` | Move cursor down |
| `a` | `add-to-attachment-list` | Add a new attachment to the attachment list |
| `o` | `open-attachment` | Open the selected attachment using the URL open command |
| `d` | `delete-from-attachment-list` | Delete the selected attachment from the attachment list |

# Attachment File Browser
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Cancel attachment file browse |
| `o` | `open-attachment` | Open the selected file using the URL open command |
| `/` | `filebrowser-begin-search` | Begin search for name in list |
| `Enter` | `filebrowser-select-file-or-enter-directory` | Select file or enter directory |
| `Space` | `filebrowser-select-current` | Select file |
| `C-b`, `PgUp` | `filebrowser-list-page-up` | Move cursor one page up |
| `C-f`, `PgDown` | `filebrowser-list-page-down` | Move cursor one page down |
| `C-u` | `filebrowser-list-half-page-up` | Move cursor one-half page up |
| `C-d` | `filebrowser-list-half-page-down` | Move cursor one-half page down |
| `g`, `Home` | `filebrowser-list-top` | Move cursor to top of list |
| `G`, `End` | `filebrowser-list-bottom` | Move cursor to bottom of list |
| `j`, `C-n`, `Down` | `filebrowser-list-next` | Move cursor down |
| `k`, `C-p`, `Up` | `filebrowser-list-previous` | Move cursor up |

# Flagged Messages
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Exit post browsing |
| `k`, `Up` | `select-up` | Select the previous message |
| `j`, `Down` | `select-down` | Select the next message |
| `f` | `flag-message` | Toggle the selected message flag |
| `Enter` | `activate-list-item` | Jump to and select current message |

# Reaction Emoji Search Window
| Keybinding | Event Name | Description |
| ---------- | ---------- | ----------- |
| `Esc`, `C-c` | `cancel` | Close the emoji search window |
| `C-p`, `Up` | `search-select-up` | Select the previous emoji |
| `C-n`, `Down` | `search-select-down` | Select the next emoji |
| `PgDown` | `page-down` | Page down in the emoji list |
| `PgUp` | `page-up` | Page up in the emoji list |
| `Enter` | `activate-list-item` | Post the selected emoji reaction |

