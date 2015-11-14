# hledger-add

A terminal UI as drop-in replacement for `hledger add`.

## Features

This project improves in the following ways on hledger's `add` command:

 - Interactive as-you-type completion for account names and
   descriptions.

 - Integrated calculator: Amounts can be written as simple sums with
   real-time feedback on the result.

 - All actions while entering a transaction can be undone

 - Dates are expected in German format `%d.%m.%Y` or abbreviated as
   `%d.%m` or simply `%d`. I intend to support other formats, but
   that's what I use and it's currently the only one implemented.

## Disclaimer

This program has been written to support *my* use case and *my*
journal. I have not tried to handle all possible cases, so it may very
well break for yours. Please feel free to report such breakage as issue.

Also: Don't look at the code :-P

## Installation
### stack

The easiest method would be [stack]: Install the [stack] program, `cd`
to `hledger-add`s source directory and type:

    stack install

To build and install all Haskell dependencies locally and install
`hledger-add` to `~/.local/bin`. See `stack --help` for more options.
You may get asked to install the GHC Haskell compiler locally. To do
that, type `stack setup`.

### Cabal

First, install the GHC Haskell compiler and the `cabal install`,
`alex` and `happy` build tools, possibly from your distribution or the
[haskell platform].

Since `cabal` builds regularly break in non-isolated environments, the
recommended next step is to create a cabal sandbox where all
dependencies will be installed in:

    cd hledger-add
	cabal sandbox init

You can now download and install all dependencies locally with

    cabal install --only-dependencies

And finally you're ready to build and install `hledger-add`:

    cabal configure --bindir ~/bin
	cabal build
	cabal copy

## Usage

`hledger-add` currently assumes that your journal is called
`~/.hledger.journal`. If you prefer something else, please make a
symlink or edit the source.

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
 6. Do you want to add this transaction to the journal

To accept the default answer, immediately press `Return` at a promt.

The following keyboard shortcuts are available:

| Key                | Function                                                                      |
| ------------------ | ----------------------------------------------------------------------------- |
| <kbd>Esc</kbd>     | Quit the program without saving the current transaction                       |
| <kbd>Ret</kbd>     | Accept the currently selected answer                                          |
| <kbd>Alt-Ret</kbd> | Accept the current answer verbatim from the text area, ignoring the selection |
| <kbd>C-z</kbd>     | Undo the last action                                                          |
| <kbd>Tab</kbd>     | Insert the currently selected answer into the text area                       |
| <kbd>C-c</kbd>     | Abort the current transaction                                                 |
| <kbd>C-n</kbd>     | Select the next context item                                                  |
| <kbd>C-p</kbd>     | Select the previous context item                                              |




[stack]: https://github.com/commercialhaskell/stack
[haskell platform]: https://www.haskell.org/platform/
