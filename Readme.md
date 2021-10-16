# Neorg Haskell Parser

Neorg Haskell Parser aims to be a complete implementation of the [Neorg markup specification](https://github.com/nvim-neorg/neorg/blob/main/docs/NFF-0.1-spec.md). Furthermore, it should be able to transform .norg files to the [pandoc](https://github.com/jgm/pandoc) AST and subsequently to the plethora of formats that pandoc supports.

## Installation

You will need ghc and cabal. Then you can build the project with:
```bash
cabal build
```

For quicker development, I have set up a bash script in `testing/ghcid.sh` which tries to parse `testing/test.norg` on every file change. It relies on the tool `ghcid`. 

## Implementation status

### Neorg Parser

#### Legend

- Implemented and tested: :heavy_check_mark:
- Implemented and not tested: :white_check_mark: 
- Partly implemented: :heavy_multiplication_x:
- Not implemented: :x:

#### Implemented features

- Attached Modifiers :heavy_multiplication_x:
- Intersecting modifier :x:
- Detached Modifiers :heavy_multiplication_x:
  - Heading :white_check_mark:
  - List :heavy_multiplication_x:
    - Unordered list :white_check_mark:
    - Ordered list :white_check_mark:
    - TODO lists :x:
  - Quote :heavy_multiplication_x:
  - Marker :x:
  - Insertions :x:
  - Definitions :x:
- Tags :heavy_multiplication_x:
  - Carryover tags :x:
  - @document.meta :white_check_mark:
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
- Trailing modifier :x:
- Escaping :x:
- Horizontal line :x:

### Transformation to Pandoc AST

Not yet started
