#!/usr/bin/env bash
GHCID="cabal exec -- ghcid"
exec $GHCID --command='ghci' --warnings --test=main "$@"
