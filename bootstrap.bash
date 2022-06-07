#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]:-$0}"; )" &> /dev/null && pwd 2> /dev/null; )";
VERSIONS=$SCRIPT_DIR/versions.json

SCALA3_VERSION="$(jq -r '.["scala3"]' $VERSIONS)"
CATS_EFFECT_VERSION="$(jq -r '.["cats-effect"]' $VERSIONS)"
mkdir -p $SCRIPT_DIR/build
pushd $SCRIPT_DIR/build

git clone --depth 1 --branch v${CATS_EFFECT_VERSION} https://github.com/typelevel/cats-effect.git
pushd cats-effect
sbt "project kernelJVM" "set version := \"${CATS_EFFECT_VERSION}\"" "++${SCALA3_VERSION} publishLocal"
popd

CATS_CORE_VERSION="$(jq -r '.["cats-core"]' $VERSIONS)"
git clone --depth 1 --branch v${CATS_CORE_VERSION} https://github.com/typelevel/cats.git
pushd cats
sbt "project coreJVM" "set version := \"${CATS_CORE_VERSION}\"" "++${SCALA3_VERSION} publishLocal"
popd
