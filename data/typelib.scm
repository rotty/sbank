((alias (name "GQuark") (target "uint32"))
  (record (name "ArgBlob")
    (field (name "name") (type "uint32"))
    (field (name "in") (type "uint") (bits 1))
    (field (name "out") (type "uint") (bits 1))
    (field (name "caller_allocates") (type "uint") (bits 1))
    (field (name "allow_none") (type "uint") (bits 1))
    (field (name "optional") (type "uint") (bits 1))
    (field (name "transfer_ownership") (type "uint")
      (bits 1))
    (field (name "transfer_container_ownership")
      (type "uint") (bits 1))
    (field (name "return_value") (type "uint") (bits 1))
    (field (name "scope") (type "uint") (bits 3))
    (field (name "reserved") (type "uint") (bits 21))
    (field (name "closure") (type "int8"))
    (field (name "destroy") (type "int8"))
    (field (name "arg_type") (type "SimpleTypeBlob")))
  (record (name "ArrayTypeBlob")
    (field (name "pointer") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 2))
    (field (name "tag") (type "uint16") (bits 5))
    (field (name "zero_terminated") (type "uint16") (bits 1))
    (field (name "has_length") (type "uint16") (bits 1))
    (field (name "has_size") (type "uint16") (bits 1))
    (field (name "array_type") (type "uint16") (bits 2))
    (field (name "reserved2") (type "uint16") (bits 3))
    (union (name "dimensions")
      (field (name "length") (type "uint16"))
      (field (name "size") (type "uint16")))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "AttributeBlob")
    (field (name "offset") (type "uint32"))
    (field (name "name") (type "uint32"))
    (field (name "value") (type "uint32")))
  (record (name "CallbackBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "signature") (type "uint32")))
  (record (name "CommonBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32")))
  (record (name "ConstantBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "type") (type "SimpleTypeBlob"))
    (field (name "size") (type "uint32"))
    (field (name "offset") (type "uint32"))
    (field (name "reserved2") (type "uint32")))
  (record (name "DirEntry")
    (field (name "blob_type") (type "uint16"))
    (field (name "local") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "offset") (type "uint32")))
  (record (name "EnumBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "storage_type") (type "uint16") (bits 5))
    (field (name "reserved") (type "uint16") (bits 9))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "n_values") (type "uint16"))
    (field (name "reserved2") (type "uint16"))
    (field (name "reserved3") (type "uint32"))
    (field (name "values")
      (type
        (array (element-type (type "ValueBlob"))
          (element-count 0)))))
  (record (name "ErrorDomainBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "get_quark") (type "uint32"))
    (field (name "error_codes") (type "uint16"))
    (field (name "reserved2") (type "uint16")))
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
  (record (name "FieldBlob")
    (field (name "name") (type "uint32"))
    (field (name "readable") (type "uint8") (bits 1))
    (field (name "writable") (type "uint8") (bits 1))
    (field (name "has_embedded_type") (type "uint8")
      (bits 1))
    (field (name "reserved") (type "uint8") (bits 5))
    (field (name "bits") (type "uint8"))
    (field (name "struct_offset") (type "uint16"))
    (field (name "reserved2") (type "uint32"))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "FunctionBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "setter") (type "uint16") (bits 1))
    (field (name "getter") (type "uint16") (bits 1))
    (field (name "constructor") (type "uint16") (bits 1))
    (field (name "wraps_vfunc") (type "uint16") (bits 1))
    (field (name "throws") (type "uint16") (bits 1))
    (field (name "index") (type "uint16") (bits 10))
    (field (name "name") (type "uint32"))
    (field (name "symbol") (type "uint32"))
    (field (name "signature") (type "uint32"))
    (field (name "is_static") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "reserved2") (type "uint16") (bits 16)))
  (alias (name "GBaseFinalizeFunc") (target "pointer"))
  (alias (name "GBaseInitFunc") (target "pointer"))
  (alias (name "GClassFinalizeFunc") (target "pointer"))
  (alias (name "GClassInitFunc") (target "pointer"))
  (record (name "GError")
    (field (name "domain") (type "GQuark"))
    (field (name "code") (type "int"))
    (field (name "message")
      (type (pointer (base-type (type "char"))))))
  (record (name "GITypelibHashBuilder"))
  (alias (name "GInstanceInitFunc") (target "pointer"))
  (alias (name "GInterfaceFinalizeFunc") (target "pointer"))
  (record (name "GInterfaceInfo")
    (field (name "interface_init")
      (type "GInterfaceInitFunc"))
    (field (name "interface_finalize")
      (type "GInterfaceFinalizeFunc"))
    (field (name "interface_data")
      (type (pointer (base-type (type "void"))))))
  (alias (name "GInterfaceInitFunc") (target "pointer"))
  (record (name "GList")
    (field (name "data")
      (type (pointer (base-type (type "void")))))
    (field (name "next")
      (type (pointer (base-type (type "GLib.List")))))
    (field (name "prev")
      (type (pointer (base-type (type "GLib.List"))))))
  (alias (name "GParamFlags") (target "uint"))
  (record (name "GParamSpec")
    (field (name "g_type_instance") (type "GTypeInstance"))
    (field (name "name")
      (type (pointer (base-type (type "char")))))
    (field (name "flags") (type "GParamFlags"))
    (field (name "value_type") (type "gtype"))
    (field (name "owner_type") (type "gtype"))
    (field (name "_nick")
      (type (pointer (base-type (type "char")))))
    (field (name "_blurb")
      (type (pointer (base-type (type "char")))))
    (field (name "qdata")
      (type (pointer (base-type (type "GData")))))
    (field (name "ref_count") (type "uint"))
    (field (name "param_id") (type "uint")))
  (record (name "GParamSpecClass")
    (field (name "g_type_class") (type "GTypeClass"))
    (field (name "value_type") (type "gtype"))
    (field (name "finalize")
      (type (pointer (base-type (type "void")))))
    (field (name "value_set_default")
      (type (pointer (base-type (type "void")))))
    (field (name "value_validate")
      (type (pointer (base-type (type "void")))))
    (field (name "values_cmp")
      (type (pointer (base-type (type "void")))))
    (field (name "dummy")
      (type
        (array
          (element-type
            (type (pointer (base-type (type "void")))))
          (element-count 4)))))
  (record (name "GParamSpecPool"))
  (record (name "GParamSpecTypeInfo")
    (field (name "instance_size") (type "uint16"))
    (field (name "n_preallocs") (type "uint16"))
    (field (name "instance_init")
      (type (pointer (base-type (type "void")))))
    (field (name "value_type") (type "gtype"))
    (field (name "finalize")
      (type (pointer (base-type (type "void")))))
    (field (name "value_set_default")
      (type (pointer (base-type (type "void")))))
    (field (name "value_validate")
      (type (pointer (base-type (type "void")))))
    (field (name "values_cmp")
      (type (pointer (base-type (type "void"))))))
  (record (name "GParameter")
    (field (name "name")
      (type (pointer (base-type (type "char")))))
    (field (name "value") (type "GValue")))
  (record (name "GSList")
    (field (name "data")
      (type (pointer (base-type (type "void")))))
    (field (name "next")
      (type (pointer (base-type (type "GLib.SList"))))))
  (record (name "GTypeCValue"))
  (record (name "GTypeClass")
    (field (name "g_type") (type "gtype")))
  (alias (name "GTypeClassCacheFunc") (target "pointer"))
  (alias (name "GTypeDebugFlags") (target "uint"))
  (alias (name "GTypeFlags") (target "uint"))
  (alias (name "GTypeFundamentalFlags") (target "uint"))
  (record (name "GTypeFundamentalInfo")
    (field (name "type_flags")
      (type "GTypeFundamentalFlags")))
  (record (name "GTypeInfo")
    (field (name "class_size") (type "uint16"))
    (field (name "base_init") (type "GBaseInitFunc"))
    (field (name "base_finalize") (type "GBaseFinalizeFunc"))
    (field (name "class_init") (type "GClassInitFunc"))
    (field (name "class_finalize")
      (type "GClassFinalizeFunc"))
    (field (name "class_data")
      (type (pointer (base-type (type "void")))))
    (field (name "instance_size") (type "uint16"))
    (field (name "n_preallocs") (type "uint16"))
    (field (name "instance_init") (type "GInstanceInitFunc"))
    (field (name "value_table")
      (type (pointer (base-type (type "GTypeValueTable"))))))
  (record (name "GTypeInstance")
    (field (name "g_class")
      (type (pointer (base-type (type "GTypeClass"))))))
  (record (name "GTypeInterface")
    (field (name "g_type") (type "gtype"))
    (field (name "g_instance_type") (type "gtype")))
  (alias (name "GTypeInterfaceCheckFunc") (target "pointer"))
  (record (name "GTypePlugin"))
  (record (name "GTypeQuery")
    (field (name "type") (type "gtype"))
    (field (name "type_name")
      (type (pointer (base-type (type "char")))))
    (field (name "class_size") (type "uint"))
    (field (name "instance_size") (type "uint")))
  (record (name "GTypeValueTable")
    (field (name "value_init")
      (type (pointer (base-type (type "void")))))
    (field (name "value_free")
      (type (pointer (base-type (type "void")))))
    (field (name "value_copy")
      (type (pointer (base-type (type "void")))))
    (field (name "value_peek_pointer")
      (type (pointer (base-type (type "void")))))
    (field (name "collect_format")
      (type (pointer (base-type (type "char")))))
    (field (name "collect_value")
      (type (pointer (base-type (type "void")))))
    (field (name "lcopy_format")
      (type (pointer (base-type (type "char")))))
    (field (name "lcopy_value")
      (type (pointer (base-type (type "void"))))))
  (record (name "GValue")
    (field (name "g_type") (type "gtype"))
    (field (name "data")
      (type
        (array
          (element-type
            (type (pointer (base-type (type "void")))))
          (element-count 2)))))
  (alias (name "GValueTransform") (target "pointer"))
  (record (name "Header")
    (field (name "magic")
      (type
        (array (element-type (type "char"))
          (element-count 16))))
    (field (name "major_version") (type "uint8"))
    (field (name "minor_version") (type "uint8"))
    (field (name "reserved") (type "uint16"))
    (field (name "n_entries") (type "uint16"))
    (field (name "n_local_entries") (type "uint16"))
    (field (name "directory") (type "uint32"))
    (field (name "n_attributes") (type "uint32"))
    (field (name "attributes") (type "uint32"))
    (field (name "dependencies") (type "uint32"))
    (field (name "size") (type "uint32"))
    (field (name "namespace") (type "uint32"))
    (field (name "nsversion") (type "uint32"))
    (field (name "shared_library") (type "uint32"))
    (field (name "c_prefix") (type "uint32"))
    (field (name "entry_blob_size") (type "uint16"))
    (field (name "function_blob_size") (type "uint16"))
    (field (name "callback_blob_size") (type "uint16"))
    (field (name "signal_blob_size") (type "uint16"))
    (field (name "vfunc_blob_size") (type "uint16"))
    (field (name "arg_blob_size") (type "uint16"))
    (field (name "property_blob_size") (type "uint16"))
    (field (name "field_blob_size") (type "uint16"))
    (field (name "value_blob_size") (type "uint16"))
    (field (name "attribute_blob_size") (type "uint16"))
    (field (name "constant_blob_size") (type "uint16"))
    (field (name "error_domain_blob_size") (type "uint16"))
    (field (name "signature_blob_size") (type "uint16"))
    (field (name "enum_blob_size") (type "uint16"))
    (field (name "struct_blob_size") (type "uint16"))
    (field (name "object_blob_size") (type "uint16"))
    (field (name "interface_blob_size") (type "uint16"))
    (field (name "union_blob_size") (type "uint16"))
    (field (name "sections") (type "uint32"))
    (field (name "padding")
      (type
        (array (element-type (type "uint16"))
          (element-count 5)))))
  (record (name "InterfaceBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 15))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "gtype_struct") (type "uint16"))
    (field (name "n_prerequisites") (type "uint16"))
    (field (name "n_properties") (type "uint16"))
    (field (name "n_methods") (type "uint16"))
    (field (name "n_signals") (type "uint16"))
    (field (name "n_vfuncs") (type "uint16"))
    (field (name "n_constants") (type "uint16"))
    (field (name "reserved2") (type "uint32"))
    (field (name "reserved3") (type "uint32"))
    (field (name "prerequisites")
      (type
        (array (element-type (type "uint16"))
          (element-count 0)))))
  (record (name "InterfaceTypeBlob")
    (field (name "pointer") (type "uint8") (bits 1))
    (field (name "reserved") (type "uint8") (bits 2))
    (field (name "tag") (type "uint8") (bits 5))
    (field (name "reserved2") (type "uint8"))
    (field (name "interface") (type "uint16")))
  (record (name "ObjectBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "abstract") (type "uint16") (bits 1))
    (field (name "fundamental") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 13))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "parent") (type "uint16"))
    (field (name "gtype_struct") (type "uint16"))
    (field (name "n_interfaces") (type "uint16"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_properties") (type "uint16"))
    (field (name "n_methods") (type "uint16"))
    (field (name "n_signals") (type "uint16"))
    (field (name "n_vfuncs") (type "uint16"))
    (field (name "n_constants") (type "uint16"))
    (field (name "reserved2") (type "uint16"))
    (field (name "ref_func") (type "uint32"))
    (field (name "unref_func") (type "uint32"))
    (field (name "set_value_func") (type "uint32"))
    (field (name "get_value_func") (type "uint32"))
    (field (name "reserved3") (type "uint32"))
    (field (name "reserved4") (type "uint32"))
    (field (name "interfaces")
      (type
        (array (element-type (type "uint16"))
          (element-count 0)))))
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
  (record (name "PropertyBlob")
    (field (name "name") (type "uint32"))
    (field (name "deprecated") (type "uint32") (bits 1))
    (field (name "readable") (type "uint32") (bits 1))
    (field (name "writable") (type "uint32") (bits 1))
    (field (name "construct") (type "uint32") (bits 1))
    (field (name "construct_only") (type "uint32") (bits 1))
    (field (name "transfer_ownership") (type "uint32")
      (bits 1))
    (field (name "transfer_container_ownership")
      (type "uint32") (bits 1))
    (field (name "reserved") (type "uint32") (bits 25))
    (field (name "reserved2") (type "uint32"))
    (field (name "type") (type "SimpleTypeBlob")))
  (record (name "RegisteredTypeBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 14))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32")))
  (record (name "Section")
    (field (name "id") (type "uint32"))
    (field (name "offset") (type "uint32")))
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
    (field (name "reserved2") (type "uint32"))
    (field (name "signature") (type "uint32")))
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
  (union (name "SimpleTypeBlob")
    (record (name "flags")
      (field (name "reserved") (type "uint") (bits 8))
      (field (name "reserved2") (type "uint") (bits 16))
      (field (name "pointer") (type "uint") (bits 1))
      (field (name "reserved3") (type "uint") (bits 2))
      (field (name "tag") (type "uint") (bits 5)))
    (field (name "offset") (type "uint32")))
  (record (name "StructBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "is_gtype_struct") (type "uint16") (bits 1))
    (field (name "alignment") (type "uint16") (bits 6))
    (field (name "foreign") (type "uint16") (bits 1))
    (field (name "reserved") (type "uint16") (bits 6))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "size") (type "uint32"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_methods") (type "uint16"))
    (field (name "reserved2") (type "uint32"))
    (field (name "reserved3") (type "uint32")))
  (record (name "UnionBlob")
    (field (name "blob_type") (type "uint16"))
    (field (name "deprecated") (type "uint16") (bits 1))
    (field (name "unregistered") (type "uint16") (bits 1))
    (field (name "discriminated") (type "uint16") (bits 1))
    (field (name "alignment") (type "uint16") (bits 6))
    (field (name "reserved") (type "uint16") (bits 7))
    (field (name "name") (type "uint32"))
    (field (name "gtype_name") (type "uint32"))
    (field (name "gtype_init") (type "uint32"))
    (field (name "size") (type "uint32"))
    (field (name "n_fields") (type "uint16"))
    (field (name "n_functions") (type "uint16"))
    (field (name "reserved2") (type "uint32"))
    (field (name "reserved3") (type "uint32"))
    (field (name "discriminator_offset") (type "int32"))
    (field (name "discriminator_type")
      (type "SimpleTypeBlob")))
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
    (field (name "invoker") (type "uint16") (bits 10))
    (field (name "reserved2") (type "uint16") (bits 6))
    (field (name "reserved3") (type "uint32"))
    (field (name "signature") (type "uint32")))
  (record (name "ValueBlob")
    (field (name "deprecated") (type "uint32") (bits 1))
    (field (name "unsigned_value") (type "uint32") (bits 1))
    (field (name "reserved") (type "uint32") (bits 30))
    (field (name "name") (type "uint32"))
    (field (name "value") (type "int32")))
  (record (name "_GITypelib")
    (field (name "data")
      (type (pointer (base-type (type "uint8")))))
    (field (name "len") (type "ulong"))
    (field (name "owns_memory") (type "boolean"))
    (field (name "mfile")
      (type (pointer (base-type (type "GMappedFile")))))
    (field (name "modules")
      (type (pointer (base-type (type "GLib.List")))))
    (field (name "open_attempted") (type "boolean"))))
