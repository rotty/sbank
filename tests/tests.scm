((systems sbank)
 (files
  ("utils.scm" (sbank utils))

  ("gobject.scm" (sbank gobject))
  
  ("stypes.scm"
   (sbank stypes) (spells foreign)
   (only (spells lists) make-list)
   (rnrs control)
   (rnrs arithmetic bitwise)
   (rnrs bytevectors)
   (rnrs lists))))
