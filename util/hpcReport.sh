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
  --exclude=Instances
  --exclude=Utils
  --exclude=Main
  $propsExclude
  --hpcdir=dist/hpc/mix/xmonad-0.12/
  dist/hpc/tix/properties/properties.tix
  "

hpc markup --destdir=dist/hpc $hpcFlags > /dev/null
echo "see dist/hpc/hpc_index.html
"
hpc report $hpcFlags
