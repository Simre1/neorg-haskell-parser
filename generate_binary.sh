cabal build -O2
cp dist-newstyle/build/x86_64-linux/ghc-9.2.5/neorg-0.1.0.0/x/neorg-pandoc/opt/build/neorg-pandoc/neorg-pandoc release/neorg-pandoc-linux86 &&
strip release/neorg-pandoc-linux86 &&
upx release/neorg-pandoc-linux86 
