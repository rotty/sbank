((record (name "Header")
   (field (name "magic")
     (type
       (array (element-type (type "int8"))
         (element-count 16))))
   (field (name "major_version") (type "uint8"))
   (field (name "minor_version") (type "uint8"))
   (field (name "reserved") (type "uint16"))
   (field (name "n_entries") (type "uint16"))
   (field (name "n_local_entries") (type "uint16"))
   (field (name "directory") (type "uint32"))
   (field (name "n_annotations") (type "uint32"))
   (field (name "annotations") (type "uint32"))
   (field (name "dependencies") (type "uint32"))
   (field (name "size") (type "uint32"))
   (field (name "namespace") (type "uint32"))
   (field (name "nsversion") (type "uint32"))
   (field (name "shared_library") (type "uint32"))
   (field (name "entry_blob_size") (type "uint16"))
   (field (name "function_blob_size") (type "uint16"))
   (field (name "callback_blob_size") (type "uint16"))
   (field (name "signal_blob_size") (type "uint16"))
   (field (name "vfunc_blob_size") (type "uint16"))
   (field (name "arg_blob_size") (type "uint16"))
   (field (name "property_blob_size") (type "uint16"))
   (field (name "field_blob_size") (type "uint16"))
   (field (name "value_blob_size") (type "uint16"))
   (field (name "annotation_blob_size") (type "uint16"))
   (field (name "constant_blob_size") (type "uint16"))
   (field (name "error_domain_blob_size") (type "uint16"))
   (field (name "signature_blob_size") (type "uint16"))
   (field (name "enum_blob_size") (type "uint16"))
   (field (name "struct_blob_size") (type "uint16"))
   (field (name "object_blob_size") (type "uint16"))
   (field (name "interface_blob_size") (type "uint16"))
   (field (name "union_blob_size") (type "uint16"))
   (field (name "padding")
     (type
       (array (element-type (type "uint16"))
         (element-count 7)))))
  (record (name "DirEntry")
    (field (name "blob_type") (type "uint16"))
    (field (name "local") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "offset") (type "uint32")))
  (union (name "SimpleTypeBlob")
    (record (field (name "reserved") (type "uint") (bits 8))
      (field (name "reserved2") (type "uint") (bits 16))
      (field (name "pointer") (type "uint") (bits 1))
      (field (name "reserved3") (type "uint") (bits 2))
      (field (name "tag") (type "uint") (bits 5)))
    (field (name "offset") (type "uint32")))
  (record (name "ArgBlob")
    (field (name "name") (type "uint32"))
    (field (name "in") (type "uint") (bits 1))
    (field (name "out") (type "uint") (bits 1))
    (field (name "dipper") (type "uint") (bits 1))
    (field (name "null_ok") (type "uint") (bits 1))
    (field (name "optional") (type "uint") (bits 1))
    (field (name "transfer_ownership") (type "uint")
      (bits 1))
    (field (name "transfer_container_ownership")
      (type "uint") (bits 1))
    (field (name "return_value") (type "uint") (bits 1))
    (field (name "reserved") (type "uint") (bits 24))
    (field (name "arg_type") (type "SimpleTypeBlob")))
  (record (name "SignatureBlob")
    (field (name "return_type") (type "SimpleTypeBlob"))
    (field (name "may_return_null") (type "uint16") (bits 1))
    (field (name "caller_owns_return_value") (type "uint16")
      (bits 1))
    (field (name "caller_owns_return_container")
      (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 13))
    (field (name "n_arguments") (type "uint16"))
    (field (name "arguments")
      (type
        (array (element-type (type "ArgBlob"))
          (element-count 0)))))
  (record (name "CommonBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32")))
  (record (name "FunctionBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "setter") (type "uint16") (bits 1))
    (field (name "getter") (type "uint16") (bits 1))
    (field (name "constructor") (type "uint16") (bits 1))
    (field (name "wraps_vfunc") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 1))
    (field (name "index") (type "uint16") (bits 10))
    (field (name "name") (type "uint32"))
    (field (name "symbol") (type "uint32"))
    (field (name "signature") (type "uint32")))
  (record (name "CallbackBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "signature") (type "uint32")))
  (record (name "InterfaceTypeBlob")
    (field (name "pointer") (type "uint8") (bits 1))
    (field (name "reserved") (type "uint8") (bits 2))
    (field (name "tag") (type "uint8") (bits 5))
    (field (name "reserved2") (type "uint8"))
    (field (name "interface") (type "uint16")))
  (record (name "ArrayTypeBlob")
    (field (name "pointer") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 2))
    (field (name "tag") (type "uint16") (bits 5))
    (field (name "zero_terminated") (type "uint16") (bits 1))
    (field (name "has_length") (type "uint16") (bits 1))
    (field (name "has_size") (type "uint16") (bits 1))
    (field (name "reserved2") (type "uint16") (bits 5))
    (union (field (name "length") (type "uint16"))
      (field (name "size") (type "uint16")))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "ParamTypeBlob")
    (field (name "pointer") (type "uint8") (bits 1))
    (field (name "reserved") (type "uint8") (bits 2))
    (field (name "tag") (type "uint8") (bits 5))
    (field (name "reserved2") (type "uint8"))
    (field (name "n_types") (type "uint16"))
    (field (name "type")
      (type
        (array (element-type (type "SimpleTypeBlob"))
          (element-count 0)))))
  (record (name "ErrorTypeBlob")
    (field (name "pointer") (type "uint8") (bits 1))
    (field (name "reserved") (type "uint8") (bits 2))
    (field (name "tag") (type "uint8") (bits 5))
    (field (name "reserved2") (type "uint8"))
    (field (name "n_domains") (type "uint16"))
    (field (name "domains")
      (type
        (array (element-type (type "uint16"))
          (element-count 0)))))
  (record (name "ErrorDomainBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "get_quark") (type "uint32"))
    (field (name "error_codes") (type "uint16"))
    (field (name "reserved2") (type "uint16")))
  (record (name "ValueBlob")
    (field (name "deprecated") (type "uint32") (bits 1))
    (field (name "reserved") (type "uint32") (bits 31))
    (field (name "name") (type "uint32"))
    (field (name "value") (type "uint32")))
  (record (name "FieldBlob")
    (field (name "name") (type "uint32"))
    (field (name "readable") (type "uint8") (bits 1))
    (field (name "writable") (type "uint8") (bits 1))
    (field (name "reserved") (type "uint8") (bits 6))
    (field (name "bits") (type "uint8"))
    (field (name "struct_offset") (type "uint16"))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "RegisteredTypeBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 14))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32")))
  (record (name "StructBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 14))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_methods") (type "uint16")))
  (record (name "UnionBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "discriminated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 13))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_functions") (type "uint16"))
    (field (name "discriminator_offset") (type "int32"))
    (field (name "discriminator_type")
      (type "SimpleTypeBlob")))
  (record (name "EnumBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 14))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "n_values") (type "uint16"))
    (field (name "reserved2") (type "uint16"))
    (field (name "values")
      (type
        (array (element-type (type "ValueBlob"))
          (element-count 0)))))
  (record (name "PropertyBlob")
    (field (name "name") (type "uint32"))
    (field (name "deprecated") (type "uint32") (bits 1))
    (field (name "readable") (type "uint32") (bits 1))
    (field (name "writable") (type "uint32") (bits 1))
    (field (name "construct") (type "uint32") (bits 1))
    (field (name "construct_only") (type "uint32") (bits 1))
    (field (name "reserved") (type "uint32") (bits 27))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "SignalBlob")
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "run_first") (type "uint16") (bits 1))
    (field (name "run_last") (type "uint16") (bits 1))
    (field (name "run_cleanup") (type "uint16") (bits 1))
    (field (name "no_recurse") (type "uint16") (bits 1))
    (field (name "detailed") (type "uint16") (bits 1))
    (field (name "action") (type "uint16") (bits 1))
    (field (name "no_hooks") (type "uint16") (bits 1))
    (field (name "has_class_closure") (type "uint16")
      (bits 1))
    (field (name "true_stops_emit") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 6))
    (field (name "class_closure") (type "uint16"))
    (field (name "name") (type "uint32"))
    (field (name "signature") (type "uint32")))
  (record (name "VFuncBlob")
    (field (name "name") (type "uint32"))
    (field (name "must_chain_up") (type "uint16") (bits 1))
    (field (name "must_be_implemented") (type "uint16")
      (bits 1))
    (field (name "must_not_be_implemented") (type "uint16")
      (bits 1))
    (field (name "class_closure") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 12))
    (field (name "signal") (type "uint16"))
    (field (name "struct_offset") (type "uint16"))
    (field (name "reserved2") (type "uint16"))
    (field (name "signature") (type "uint32")))
  (record (name "ObjectBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "parent") (type "uint16"))
    (field (name "n_interfaces") (type "uint16"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_properties") (type "uint16"))
    (field (name "n_methods") (type "uint16"))
    (field (name "n_signals") (type "uint16"))
    (field (name "n_vfuncs") (type "uint16"))
    (field (name "n_constants") (type "uint16"))
    (field (name "interfaces")
      (type
        (array (element-type (type "uint16"))
          (element-count 0)))))
  (record (name "InterfaceBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "n_prerequisites") (type "uint16"))
    (field (name "n_properties") (type "uint16"))
    (field (name "n_methods") (type "uint16"))
    (field (name "n_signals") (type "uint16"))
    (field (name "n_vfuncs") (type "uint16"))
    (field (name "n_constants") (type "uint16"))
    (field (name "prerequisites")
      (type
        (array (element-type (type "uint16"))
          (element-count 0)))))
  (record (name "ConstantBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "type") (type "SimpleTypeBlob"))
    (field (name "size") (type "uint32"))
    (field (name "offset") (type "uint32")))
  (record (name "AnnotationBlob")
    (field (name "offset") (type "uint32"))
    (field (name "name") (type "uint32"))
    (field (name "value") (type "uint32")))
  (record (name "GTypelib")
    (field (name "data")
      (type (array (element-type (type "uint8")))))
    (field (name "len") (type "size_t"))
    (field (name "owns_memory") (type "boolean"))
    (field (name "mfile")
      (type
        (pointer (base-type (type "GMappedFile")) (depth 1)
          (size 4) (alignment 4))))
    (field (name "modules")
      (type
        (pointer (base-type (type "GList")) (depth 1)
          (size 4) (alignment 4))))))