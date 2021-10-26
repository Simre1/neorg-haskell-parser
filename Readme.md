# Neorg Haskell Parser

Neorg Haskell Parser aims to be a complete implementation of the [Neorg markup specification](https://github.com/nvim-neorg/neorg/blob/main/docs/NFF-0.1-spec.md). Furthermore, it should be able to transform .norg files to the [pandoc](https://github.com/jgm/pandoc) AST and subsequently to the plethora of formats that pandoc supports.

## Installation

You will need ghc and cabal. Then you can build the project with:
```bash
cabal build
```

You can run the tests with:
```bash
cabal test
```

<!-- For quicker development, I have set up a bash script in `testing/ghcid.sh` which tries to parse `testing/test.norg` on every file change. It relies on the tool `ghcid`.  -->

## Implementation status

### Neorg Parser

#### Legend

- Implemented and tested: :heavy_check_mark:
- Almost complete: :white_check_mark: 
- Partly implemented: :heavy_multiplication_x:
- Not implemented: :x:

#### Tokenizer

The Tokenizer is implemented in Neorg.Token

- Attached Modifiers :white_check_mark:
- Intersecting modifier :x:
- Detached Modifiers :heavy_multiplication_x:
  - Heading :heavy_check_mark:
  - List :heavy_check_mark:
    - Unordered list :heavy_check_mark:
    - Ordered list :heavy_check_mark:
    - TODO lists :heavy_check_mark:
  - Quote :heavy_multiplication_x: 
  - Marker :heavy_multiplication_x: 
  - Insertions :heavy_multiplication_x:
  - Definitions :heavy_multiplication_x:
- Tags :x:
  - Carryover tags :x:
  - @document.meta :x:
  - @comment :x:
  - @ToC :x:
  - @ordered :x:
  - @color :x:
  - @name :x:
  - @table :x:
  - @embed :x:
  - @code :x:
  - @math :x:
  - Custom :x:
- Trailing modifier :white_check_mark:
- Escaping :x:
- Horizontal line :x:

#### Parser

Not yet started

### Transformation to Pandoc AST

Not yet started
