#!/bin/sh

# Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

# Author: Andreas Rottmann <a.rottmann@gmx.at>

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with this library. If not, see
# <http://www.gnu.org/licenses/>.

GLIB_INCLUDEDIR=`pkg-config --variable=includedir glib-2.0`/glib-2.0
GLIB_LIBDIR=`pkg-config --variable=libdir glib-2.0`
GI_HOME="$HOME/src/g-i"

scan_files=""
for h in \
    glib/gerror.h glib/glist.h glib/gslist.h glib/gquark.h \
    gobject/gvalue.h gobject/gparam.h gobject/gtype.h; do
    scan_files="$scan_files $GLIB_INCLUDEDIR/$h"
done
scan_files="$scan_files $GI_HOME/girepository/gitypelib-internal.h"

g-ir-scanner -n gtypelib \
    --symbol-prefix="" \
    --identifier-prefix="" \
    --output data/typelib.gir \
    --library gobject-2.0 \
    --pkg gobject-introspection-1.0 \
    -I"$GLIB_INCLUDEDIR" \
    -I"$GLIB_LIBDIR"/glib-2.0/include \
    -DGOBJECT_COMPILATION \
    --c-include="glib.h" \
    --c-include="glib-object.h" \
    $scan_files
ikarus --r6rs-script scripts/dump-typelib.sps data/typelib.gir > data/typelib.scm
