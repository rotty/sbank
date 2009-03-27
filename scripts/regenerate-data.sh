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

GI_HOME="$HOME/src/g-i"

g-ir-scanner -n gtypelib --library gobject-2.0 --pkg gobject-introspection-1.0 \
    -o data/typelib.gir "$GI_HOME/girepository/gtypelib.h"
ikarus --r6rs-script scripts/dump-typelib.sps < data/typelib.gir > data/typelib.scm
