# Neorg Haskell Parser

Neorg Haskell Parser aims to be a complete implementation of the [Neorg markup specification](https://github.com/nvim-neorg/norg-specs). Furthermore, it should be able to transform .norg files to the [pandoc](https://github.com/jgm/pandoc) AST and subsequently to the plethora of formats that pandoc supports.

## Installation

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

## Usage

After having aquired a binary of `neorg-pandoc`, you can use it to transform Neorg files into Pandoc json documents. `neorg-pandoc` expects exactly one parameter, which should be the file path to the Neorg file. If parsing was successful, the json document is printed to stdout.

An example usage might be:
```bash
./neorg-pandoc-linux86 testing/test.norg | pandoc -f json -o testing/out.pdf
```

Here, I have used the `neorg-pandoc-linux86` binary to transform my `norg` file into the pandoc `json` format. The result is piped into pandoc to create a pdf. The `-f json` argument tells pandoc that the input is a `json` file  and `-o testing/out.pdf` is the location of the `pdf` file. It is also possible to transform your `norg` file into other formats; for example to markdown by replacing `-o testing/out.pdf` with `-o testing/out.md`. For further options refer to the `pandoc` documentation.

## Implementation status

- Attached Modifiers :heavy_check_mark: 
- Intersecting modifiers :heavy_check_mark: 
- Detached Modifiers :white_check_mark:
  - Heading :heavy_check_mark:
  - List :white_check_mark:
    - Unordered lists :heavy_check_mark:
    - Ordered lists :heavy_check_mark:
    - TODO lists :heavy_check_mark:
    - Other list types: :x:
  - Quotes :heavy_check_mark: 
  - Markers :heavy_check_mark: 
  - Insertions :x:
- Definitions :heavy_check_mark:
- Tags :white_check_mark:
  - Carryover tags :x:
  - @document.meta :white_check_mark:
  - @comment :heavy_check_mark:  
  - @ToC :x:
  - @ordered :x:
  - @color :x:
  - @name :x:
  - @table :heavy_check_mark:  
  - @embed :heavy_check_mark: 
  - @code :heavy_check_mark:
  - @math :heavy_check_mark: 
  - @image :x:
  - Custom :x:
- Trailing modifiers :heavy_check_mark:
- Escaping :heavy_check_mark:
- Horizontal line :heavy_check_mark: 
- Links :heavy_check_mark:
- Anchors :x:

### Legend

- Implemented: :heavy_check_mark:
- Work in Progress: :white_check_mark: 
- Not implemented: :x:




