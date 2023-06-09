#! /usr/bin/env bash

set -ex

name="Deliverance"
t=$(date --rfc-3339=seconds | sed 's/ /-/g' | sed 's/:/-/g')
g=$(git rev-parse --short HEAD)
outdir="jam-windows-$t-$g"
releasework="releases/windows-$t-$g"
releasedir="$releasework/$name"

nix-build -A projectCross.mingwW64.hsPkgs.ld53.components.exes.ld53-exe -o "$outdir" ./default.nix

mkdir -p "$releasedir"/resources

cp "$outdir"/bin/*.exe "$releasedir"
cp "$outdir"/bin/*.dll "$releasedir"

rm "$outdir"

cp -r resources/* "$releasedir/resources/"

# ghc-pkg doesn't help Nix discover the dependencies on these dlls, so we manually copy them
#cp -Pn "$(nix-build --no-out-link --expr '(import ../../../nix/pkgs.nix).pkgsCross.mingwW64.zlib')/bin"/*.dll "$releasedir"

chmod +w -R "$releasedir"

pushd "$releasework"
zip -r windows-$name.zip $name
popd

echo "Built $releasedir"
