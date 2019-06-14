#!/usr/bin/env bash

CTX=~/screenshots/$(ls -1 ~/Pictures/screens --hide current-context | dmenu)
mkdir -p "$CTX"
ln -Tsf "$CTX" ~/Pictures/screens/current-context
