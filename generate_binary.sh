cabal build -O2
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/neorg-0.1.0.0/x/neorg-pandoc/build/neorg-pandoc/neorg-pandoc release/neorg-pandoc-linux86 &&
strip release/neorg-pandoc-linux86 &&
upx release/neorg-pandoc-linux86 
