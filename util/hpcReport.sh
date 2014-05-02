#!/bin/bash

if [ ! -e xmonad.cabal ]; then
  echo "run in the same dir as xmonad.cabal after having run

      cabal configure --enable-tests --enable-library-coverage; cabal test

        "
  exit 1
fi

for action in markup report; do
  hpc $action \
    --exclude=Instances \
    --exclude=Utils \
    --exclude=Main \
    $(find tests/Properties -name '*.hs' | sed -e 's_/_._g' -e 's_.hs$__' -e 's_^tests._--exclude=_' ) \
    --hpcdir=dist/hpc/mix/xmonad-0.12/  \
    dist/hpc/tix/properties/properties.tix
done
