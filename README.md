# hledger-iadd

An interactive terminal UI as drop-in replacement for `hledger add`.

![Screencast](doc/screencast.gif)

## Features

This project improves in the following ways on hledger's `add` command:

 - Interactive as-you-type completion for account names and
   descriptions with optional fuzzy matching (see [below](#configuration-file)).

 - Integrated calculator: Amounts can be written as simple sums with
   real-time feedback on the result.

 - All actions while entering a transaction can be undone

 - Configurable format for date input. Instead of `y/m/d` it is also
   possible to use other formats like the german `d.m.y`.

Also see the user guide under [Usage](#usage).

## Installation
### Latest release
#### stack

The easiest method would be [stack]: Install the [stack] program and
type:

    stack install --resolver=nightly hledger-iadd

This downloads and builds `hledger-iadd` and all it's Haskell
dependencies. After that, it copys the resulting binary to
`~/.local/bin`. See `stack --help` for more options. You may get asked
to install the GHC Haskell compiler locally. To do that, type `stack
setup`.

#### cabal

First, install the GHC Haskell compiler and the `cabal install`,
`alex` and `happy` build tools, possibly from your distribution or the
[haskell platform]. Type

    cabal install --bindir ~/bin hledger-iadd

to install the binary in `~/bin`.

### Development version

To install the development version, clone the git repository first:

    git clone https://github.com/hpdeifel/hledger-iadd.git
	cd hledger-iadd

#### stack

The easiest method would be [stack]: Install the [stack] program and
type:

    stack install

To build and install all Haskell dependencies locally and install
`hledger-iadd` to `~/.local/bin`. See `stack --help` for more options.
You may get asked to install the GHC Haskell compiler locally. To do
that, type `stack setup`.

#### Cabal

First, install the GHC Haskell compiler and the `cabal install`,
`alex` and `happy` build tools, possibly from your distribution or the
[haskell platform].

Since `cabal` builds regularly break in non-isolated environments, the
recommended next step is to create a cabal sandbox where all
dependencies will be installed in:

    cd hledger-iadd
	cabal sandbox init

You can now download and install all dependencies locally with

    cabal install --only-dependencies

And finally you're ready to build and install `hledger-iadd`:

    cabal configure --bindir ~/bin
	cabal build
	cabal copy

## Usage

You can start the program either with

    hledger iadd

or simply `hledger-iadd`.

The following command line options are available:

  - `-f/--file/`: Path to the journal file. (Default: `~/.hledger.journal`)
  - `--date-format`: Format for parsing dates. (Default:
    `[[%y/]%m/]%d`, the usual ledger date format). Brackets can be
    used to specify optional parts. E.g the german date format would
    be `%d[.[%m[.[%y]]]]`. (Dates are written as `y/m/d` to the
    journal regardless of this option).
  - `--completion-engine`: Algorithm for account name completion. Can
    be `substrings` or `fuzzy`.
  - `--dump-default-config`: Print the example config file to stdout
    and exit

The UI is partitioned in 4 regions:

    Current Transaction (view of your work in progress)
	---------------------------------------------------
	Question: [ text area                             ]
	---------------------------------------------------
	Context information (e.g. list of accounts)




	---------------------------------------------------
	Message area

For each transaction, you will get asked the following questions in
order:

 1. Date?
 2. Description?
 3. Account name?
 4. Amount?
 5. The last two questions are repeated until you enter the empty account
 6. Do you want to add this transaction to the journal?

To accept the default answer, immediately press <kbd>Return</kbd> at a
promt.

While you type, the context area shows possible completions. Pressing
<kbd>Return</kbd> answers the question with the currently selected
completion. You can select differnt completions with <kbd>C-n</kbd>
and <kbd>C-p</kbd>.

The following keyboard shortcuts are available:

| Key                             | Function                                                                      |
| ------------------------------- | ----------------------------------------------------------------------------- |
| <kbd>C-c</kbd>, <kbd>C-d</kbd>  | Quit the program without saving the current transaction                       |
| <kbd>Esc</kbd>                  | Abort the current transaction or exit when at toplevel                        |
| <kbd>Ret</kbd>                  | Accept the currently selected answer                                          |
| <kbd>Alt-Ret</kbd>              | Accept the current answer verbatim from the text area, ignoring the selection |
| <kbd>C-z</kbd>                  | Undo the last action                                                          |
| <kbd>Tab</kbd>                  | Insert the currently selected answer into the text area                       |
| <kbd>C-n</kbd>,<kbd>↓</kbd>     | Select the next context item                                                  |
| <kbd>C-p</kbd>,<kbd>↑</kbd>     | Select the previous context item                                              |
| <kbd>;</kbd>                    | Edit comment for current prompt                                               |
| <kbd>Alt-;</kbd>                | Edit transaction comment                                                      |
| <kbd>F1</kbd>,<kbd>Alt-?</kbd>  | Show help dialog                                                              |

## Default Currency

To make entry easier it is recommended that you set a [default commodity](http://hledger.org/manual.html#default-commodity)
in your ledger file if you haven't already done so.
That way when entering amounts, `hledger-iadd` will add the symbols for you.
You can do this by adding a line like below to the top of your ledger file:

```lisp
; sets the default commodity symbol and placement, thousands separator, and decimal symbol
D $1,000.00
```

## Configuration File

`hledger-iadd` is optionally configurable through a configuration file
in `${XDG_CONFIG_HOME}/hledger-iadd/config.conf`. This file consists
of simple

    key = value

assignments on individual lines with whitespace or comments starting
with `#` between them. The default config can be obtained by
passing `--dump-default-config` to `hledger-iadd`.

The following options are currently available:

  - `file`: Path to the journal file.
  - `date-format`: The date format. See the documentation for
    `--date-format` for details.
  - `completion-engine`: Algorithm used to find completions for
    account names. Possible values are:
	- `substrings`: Every word in the search string has to occur
      somewhere in the account name
	- `fuzzy`: All letters from the search string have to appear in
      the name in the same order


[stack]: https://github.com/commercialhaskell/stack
[haskell platform]: https://www.haskell.org/platform/
