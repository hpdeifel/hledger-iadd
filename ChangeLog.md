# 1.1.1

 - bugfix: Show cursor in empty entry widget
 - bugfix: Correctly execute `--help` and `--dump-default-config` in
   the presence of syntax errors in the config file

# 1.1

 - Add a configuration file for persistent settings
 - Disallow unbalanced transactions
 - Order postings naturally and omit balancing amounts in transaction preview (thanks Tristan Hume)
 - Suggest account based on last transaction if no similar transaction is found (thanks Tristan Hume)
 - Make completed dates as recent as possible (thanks Thorsten Wißmann)
 - Optional fuzzy matching via config option "completion-engine" (thanks Tristan Hume)
 - Add Ctrl-d as new keybinding for 'quit'
 - Make ESC quit at the toplevel
 - Various bug fixes

# 1.0

 - Initial release
