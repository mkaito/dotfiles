; extends

; Keyword arguments (implicit hash) inside a function call
(argument_list
  (pair
    key: (hash_key_symbol)
    value: (_) @kwarg.value) @kwarg.outer)
