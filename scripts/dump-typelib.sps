(import (rnrs base)
        (rnrs io simple)
        (spells pretty-print)
        (sbank typelib gir))

(pretty-print (gir-xml->stype-list (current-input-port)))
