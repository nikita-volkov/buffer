#!/bin/bash
set -eo pipefail

cabal build benchmarks
cd dist
build/benchmarks/benchmarks "100" +RTS -N -p -s -i0.0001 -RTS
open benchmarks.prof
