cabal build -O2
cp dist-newstyle/build/x86_64-linux/ghc-9.2.5/neorg-0.1.0.0/x/neorg-pandoc/opt/build/neorg-pandoc/neorg-pandoc release/neorg-pandoc-linux86 &&
strip release/neorg-pandoc-linux86 &&
upx release/neorg-pandoc-linux86 
rm release/neorg-pandoc-linux86.zip
zip release/neorg-pandoc-linux86.zip release/neorg-pandoc-linux86
rm release/neorg-pandoc-linux86

gh release delete latest -y
gh release create latest 'release/neorg-pandoc-linux86.zip#Linux x86' -p -t Latest --notes "This release corresponds to the newest commit of the main branch"
