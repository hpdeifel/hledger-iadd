# 1.2.2

  - Support for megaparsec-5.3.0
  - Bump brick dependency to 0.19

# 1.2.1

  - Support for hledger-lib-1.2
  - Minor documentation fixes

# 1.2

  - Add support for comments (bound to `;`)
  - Restore previous text input on undo
  - Bump text-zipper dependency to 0.10

# 1.1.4

  - Sort account names by frequency for completion
  - Bind Home/End im entry field
  - Bump brick and vty dependencies

# 1.1.3

  - Add more emacs/readline like keybindings in entry field (`C-f`/`C-b`,
    `M-f`/`M-b`, `M-Del`/`C-w`, `M-d`)
  - Fix account suggestion order to be more like `hledger add`

# 1.1.2

 - Respect ${LEDGER_FILE} environment variable
 - Add --version command
 - Bump brick dependency to 0.15.2
 - Bump hledger-lib dependency to 1.1
 - Bind C-u to 'delete to beginning of line'

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
