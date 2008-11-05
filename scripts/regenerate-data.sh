#!/bin/sh

GI_HOME="$HOME/src/g-i"

g-ir-scanner -n gtypelib --library gobject-2.0 --pkg gobject-introspection-1.0 \
    -o data/typelib.gir "$GI_HOME/girepository/gtypelib.h"
ikarus --r6rs-script scripts/dump-typelib.sps < data/typelib.gir > data/typelib.scm
