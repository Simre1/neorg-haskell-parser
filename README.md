# Neorg Haskell Parser

Reimplementation of the parser with the [Neorg markup specification](https://github.com/nvim-neorg/norg-specs). It will be able to transform .norg files to the [pandoc](https://github.com/jgm/pandoc) AST and subsequently to the plethora of formats that pandoc supports.

## Parser Implementation State

I will focus on the first three layers for now. Layers 4 and 5 are on hold for now.

- [x] Layer 1: Paragraphs
- [ ] Layer 2:
  - [x] Heading
  - [x] Lists
  - [x] Quotes
  - [ ] More link locations
  - [ ] Verbatim Ranged tags
  - [ ] Delimiting modifiers
- [ ] Layer 3
- [ ] PDF creation

## Pandoc Implementation State

- [ ] Layer 1: Paragraphs
- [ ] Layer 2:
  - [ ] Heading
  - [ ] Lists
  - [ ] Quotes
  - [ ] More link locations
  - [ ] Verbatim Ranged tags
  - [ ] Delimiting modifiers
- [ ] Layer 3
- [ ] PDF creation


## Installation (not working yet)

### Using a precompiled binary

I have added a compiled binary for linux in the [releases section of github](https://github.com/Simre1/neorg-haskell-parser/releases), which should work on most x86 linux systems.

### Building from Source

You will need ghc and cabal. Then you can build the project with:
```bash
cabal build neorg-pandoc
``` 

You can run the tests with:
```bash
cabal run test
```

## Usage (not working yet)

After having aquired a binary of `neorg-pandoc`, you can use it to transform Neorg files into Pandoc json documents. `neorg-pandoc` expects exactly one parameter, which should be the file path to the Neorg file. If parsing was successful, the json document is printed to stdout.

An example usage might be:
```bash
./neorg-pandoc-linux86 testing/test.norg | pandoc -f json -o testing/out.pdf
```

Here, I have used the `neorg-pandoc-linux86` binary to transform my `norg` file into the pandoc `json` format. The result is piped into pandoc to create a pdf. The `-f json` argument tells pandoc that the input is a `json` file  and `-o testing/out.pdf` is the location of the `pdf` file. It is also possible to transform your `norg` file into other formats; for example to markdown by replacing `-o testing/out.pdf` with `-o testing/out.md`. For further options refer to the `pandoc` documentation.
