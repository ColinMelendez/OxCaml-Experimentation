# Bonsai Term Clipboard

`bonsai_term_clipboard` is a library that lets you both "get" the clipboard contents
and also "set" the clipboard contents.

There's not a single, canonical way of copying into the clipboard that works
across all terminal emulators, so this library uses a mix of strategies.

When "getting" the clipboard we rely solely on `xclip`.

When "setting" the clipboard we attmept to perform a copy using:

- `xclip`.
- `OSC 52`.
- (soon) Using remote controller. (so that it'll work when in windows / when inside of
  putty.)


Right now this library should work everywhere except in the embedded VS Code terminal and inside of putty.
After we start using remote controller, it should work in putty / windows. The VS Code support is 
blocked on a bug in VS Code.
