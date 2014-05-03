#!/bin/bash

set -e

if [[ ! ( -e xmonad.cabal && -e dist/hpc/tix/properties/properties.tix ) ]]; then
  echo "run in the same dir as xmonad.cabal after having run

      cabal configure --enable-tests --enable-library-coverage; cabal test

        "
  exit 1
fi


propsExclude=$(find tests/Properties -name '*.hs' \
        | sed -e 's_/_._g' -e 's_.hs$__' -e 's_^tests._--exclude=_' )

hpcFlags="
  --hpcdir=dist/hpc/mix/
  dist/hpc/tix/properties/properties.tix
  "


if [[ ! (-e dist/hpc/mix/Main.mix) ]]; then
  mv dist/hpc/mix/properties/* dist/hpc/mix/
  mv dist/hpc/mix/xmonad-*/xmonad-*/* dist/hpc/mix/xmonad-*/
fi


hpc markup --destdir=dist/hpc $hpcFlags > /dev/null
echo "see dist/hpc/hpc_index.html
"
hpc report $hpcFlags
