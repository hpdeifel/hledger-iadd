# 1.3.13   [2021-03-10]

  - dependencies: Support (and require) hledger-lib-1.21
  - dependencies: Support megaparsec-9

# 1.3.12   [2020-08-31]

  - dependencies: Fix tests build with hledger-lib-1.19

# 1.3.11   [2020-06-04]

  - bugfix: Fix check for balanced transactions in the presence of commodities.
  - dependencies: Fix build with hledger-lib-1.18

# 1.3.10   [2020-01-14]

  - dependencies: Support megaparsec-8

# 1.3.9   [2019-03-02]

  - dependencies: Port to hledger-lib-1.14
  - Add AUR packaging

# 1.3.8

  - dependencies: Port to hledger-lib-1.13

# 1.3.7

  - feature: Add abbreviated days of the week to date completion (e.g. `mon`,
    `tue`, etc)
  - dependencies: Port to hledger-lib-1.12 and megaparsec-7
  - dependencies: Support GHC-8.6
  - dependencies: Switch stack builds to ghc 8.4 by default

# 1.3.6

  - bugfix: Use local time instead of UTC everywhere

# 1.3.5

  - Fix build with hledger-lib 1.9.1

# 1.3.4

  - Fix test suite build with hledger-lib 1.9

# 1.3.3

  - Support new dependencies
  - Raise lower bound on hledger-lib to 1.5

# 1.3.2

  - Highlight currently constructed transaction
  - Fix build with GHC 8.4
  - Support new dependencies
  - Drop dependency on text-format

# 1.3.1

  - Support brick <= 0.32
  - Support megaparsec <= 6.4

# 1.3.0

  - Detect duplicate transactions and warn about them
  - Add empty line before transactions when writing to journal
  - Don't elide the last amount in transactions
  - Support account directive for account completion
  - Bugfixes and dependency bumps

# 1.2.6

  - Fix build with hledger-lib >= 1.3.1
  - Support for megaparsec-6.1
  - Support for brick <= 0.24
  - Fix test suite with ghc 8.2

# 1.2.5

  - Fix broken release tarball

# 1.2.4

  - Support for megaparsec-6.0

# 1.2.3

  - Support for brick-0.20
  - Restore compatibility with brick-0.17
  - Support for hledger-lib-1.3

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

<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
