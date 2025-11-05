
# Frequently Asked Questions

## How can I tell which messages are part of threads?

There are two ways that Matterhorn indicates this visually. Messages
that are part of ongoing threads will be rendered with a vertical bar
before the post's author. In addition, if the messages in a thread
follow the thread's first post directly, they'll be rendered underneath
the original post as follows:

```
user1: hello!
│ user2: hello to you, too!
```

But if the message is part of a thread that is interspersed with other
activity in the channel, those messages will display a portion of the
thread's original post above them, grayed out:

```
user1: first line of user1's original message...
│ user2: new message in thread
```

To view all messages in a thread by themselves in a dedicated window,
see the default ``t`` key binding in message selection mode (``C-s`` by
default).

## Does matterhorn support Gitlab authentication?

Matterhorn supports GitLab authentication indirectly. In order to use
Matterhorn with GitLab authentication, see the Authentication section on
Matterhorn details for use of session tokens.

## Does matterhorn support Mattermost Personal Access Tokens?

Yes. See the Authentication section above.

## How can I get Matterhorn to render strikethrough Markdown text?

Your terminal's terminfo settings may not expose the right capabilities
to draw strikethrough text. The necessary capabilities are `smxx` (begin
strikethrough) and `rmxx` (end strikethrough) and can be found by
looking at the output of `infocmp -a`.

To ensure that Matterhorn knows how to render strikethrough text
for your terminal, you may need to modify your terminfo settings to
advertise the right capabilities. The steps to do this are:

1. Make a new terminfo file based on your current value of `TERM`.
   For example, if `TERM` is `screen-256color`, then a good name is
   `screen-256color-strike.terminfo`. Then add the capabilities to the
   file and be sure to set `use` to the current value of `TERM`:
   ```
   screen-256color-strike|screen with 256 colors and strikethrough,
     smxx=\E[9m, rmxx=\E[29m,
       use=screen-256color,
   ```
1. Then install the new terminfo profile with
   ```
   tic -x screen-256color-strike.terminfo
   ```
1. Finally, run `matterhorn` with `TERM` set to `screen-256color-strike`.

If your terminal does not support strikethrough, we recommend
customizing the `markdownStrikethrough` theme attribute so that it has a
different color to distinguish it from normal text, e.g.,

```
[other]
markdownStrikethrough.fg = red
```

## How can I get Matterhorn to render emphasized Markdown text with an italic font?

In `~/.config/matterhorn/theme.ini`,
```
[other]
markdownEmph.style = [italic]
```
and in `~/.config/matterhorn/config.ini`,
```
themeCustomizationFile: theme.ini
```

This is known to work on `gnome-terminal` version `3.32.2`, VTE version
`0.56.3`; it may work for you, too. Many terminal emulators do not
support italics at all or without various hacks. Let us know what works
for you!

## I enabled italicized text in my theme configuration. Why doesn't it work?

Most terminfo files for typical terminal configurations do not provide
support for italicized text. If your terminal emulator supports italics,
you must enable it in your terminfo database in order to use it in
Matterhorn. For more information, see these links:

* http://www.nerdyweekly.com/posts/enable-italic-text-vim-tmux-gnome-terminal/
* https://medium.com/@dubistkomisch/how-to-actually-get-italics-and-true-colour-to-work-in-iterm-tmux-vim-9ebe55ebc2be
* https://github.com/tmux/tmux/blob/2.1/FAQ#L355-L383

## I am seeing malformed characters or display corruption when I run matterhorn in my terminal. What could be causing this?

Some terminal emulators cannot handle the extra escaping that occurs
when the URL hyperlinking mode is enabled. Try setting `hyperlinkUrls =
False` in your `config.ini` file.

## Does Matterhorn support emoji?

There are two kinds of emoji in Mattermost: Unicode-based emoji
and custom, graphical emoji.

Matterhorn has partial support for Unicode-based emoji. The support
is partial due to the lack of consistent support for wide Unicode
characters in various terminal emulators and in Matterhorn itself.
Results may vary, and use of Unicode-based emoji characters may
cause terminal rendering issues depending on the terminal emulator
and character(s) in use. However, Matterhorn offers a configuration
workaround for this for specific characters that cause problems. See the
"Configuring Wide Unicode Characters" section of the README for details.
For some technical information on the problem of Unicode in terminals,
see this publication:

https://www.jeffquast.com/post/state-of-terminal-emulation-2025/

Matterhorn does not support custom server-defined emoji since it does
not have support for rendering graphics in the terminal; these will be
rendered in their text form (":...:").

## I'm running Matterhorn in Tmux. How can I paste tmux buffers into Matterhorn's editor?

By default, `tmux`'s `paste-buffer` binding, `prefix-]`, pastes buffer
text by replaying it as terminal input. This will cause unwanted
behavior when Matterhorn receives that text and sends out each input
line as a separate message. But `tmux` supports bracketed paste mode
to make a `tmux` paste a block of text as *one* Matterhorn message.
It can be enabled by changing the behavior of `prefix-]` in the Tmux
configuration:

```
unbind-key -T prefix ]
bind-key -T prefix ] paste-buffer -p
```

## I'm using Mattermost through an Nginx proxy and I keep getting disconnected.

You might need to adjust your Nginx proxy settings.
For context, see [this potentially related report](https://github.com/matterhorn-chat/matterhorn/issues/578).

## Why doesn't Matterhorn support my desired keybindings?

In many terminal emulators, some key and modifier combinations do not
work at all or do not behave as expected. In these cases it will be
necessary to configure Matterhorn to intepret the appropriate escape
sequences in order to translate them into the desired key and modifier
combinations.

To configure Matterhorn to support a particular key combination, some
steps must be followed. Here are the steps, using `Control-Backspace` as
the example key combination (which is commonly not supported):

1) Determine the escape sequence that your terminal sends for the
   desired key combination. This can usually be done by consulting
   the terminal settings or by running `cat >/dev/null`, pressing the
   desired key combination, and observing the escape sequence that is
   printed. For example, for `Control-Backspace` on my terminal I get:
   ```
   $ cat >/dev/null
   ^[[3;5~
   ```
   This indicates that the sequence is `\ESC[3;5~`.
2) Create a file `~/.vty/config` and add to it the line `map _
   "\ESC[3;5~" KBS [MCtrl]`. This configuration file is loaded by
   the `vty` library, the library that Matterhorn uses to translate
   terminal input into key events. This configuration line tells
   `vty` to translate the input sequence `\ESC[3;5~` to the `KBS`
   (backspace) key with the modifier list `[MCtrl]` (control) regardless
   of the value of `TERM` (underscore). The list of valid keys and
   modifiers usable in `map` lines can be found [in the vty Haskell
   documentation](http://hackage.haskell.org/package/vty-5.28.2/docs/Graphics-Vty-Input.html).
3) Set up the desired event in `~/.config/matterhorn/config`'s
   `[keybindings]` section to be triggered by `C-Backspace`.

Another way to get the input byte sequence associated with your
keybinding is to run `matterhorn` with Vty debug logging enabled:

```
VTY_DEBUG_LOG=/tmp/matterhorn-keys-debug matterhorn
```

While `matterhorn` is running, the Vty debug log will contain output
about each received key combination like:

```
input bytes: "\b"
```

that will indicate what should go into the `map` entries in
`~/.vty/config`.

## Will you add support for my favorite GNU `readline` key to the message editor?

Please open a ticket to discuss support for any editing features that
you would like to see added. But please bear in mind that while we do
want Matterhorn's built-in text editor to provide a reasonable feature
set for editing messages, we do not intend to support all features
supported by other applications, and Matterhorn's editor is *not*
intended to provide the full feature set provided by `readline` even
though some of its features and behaviors are informed by `readline`.
This is also why we provide the ability to invoke an application of your
choosing to edit messages with `M-k` (`invoke-editor`).

## Matterhorn's default bindings for some feature don't work in my environment. What do I do?

The default binding for a Matterhorn feature might collide with bindings
in your operating system's settings, might trigger your terminal
emulator to generate an input sequence that Matterhorn doesn't know
about, or just not be supported by your terminal emulator at all. In
these cases you might need to customize your key bindings to ones that
work in your environment, either by disabling operating system-level
bindings to reduce clashes or by providing a custom key binding
configuration to Matterhorn.

To configure Matterhorn to use different bindings, you can add something
like the following to your Matterhorn configuration file:

```ini
[keybindings]
prev-team = S-Left
next-team = S-Right
```

See `/help keybindings` for more information on how to configure
customized keybindings.
