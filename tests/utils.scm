(testeez "scheme-ified-symbol"
  (test/equal "plain" (scheme-ified-symbol "foobar") 'foobar)
  (test/equal "uscores" (scheme-ified-symbol "foo_bar_1") 'foo-bar-1)
  (test/equal "all uppercase" (scheme-ified-symbol "THIS_IS_A_CONSTANT") '*this-is-a-constant*)
  (test/equal "camelcase - one word" (scheme-ified-symbol "Window") '<window>)
  (test/equal "camelcase - 3 words" (scheme-ified-symbol "FooDialogBar") '<foo-dialog-bar>))

(testeez "c-ified-string"
  (test/equal "plain" (c-ified-string 'foobar) "foobar")
  (test/equal "dashes" (c-ified-string 'foo-bar-1) "foo_bar_1")
  (test/equal "stars" (c-ified-string '*this-is-a-constant*) "THIS_IS_A_CONSTANT")
  (test/equal "angle brackets - one word" (c-ified-string '<window>) "Window")
  (test/equal "angle brackets - 3 words" (c-ified-string '<foo-dialog-bar>) "FooDialogBar"))

(testeez "name-symbol/prefix"
  (test/equal "plain" (name-symbol/prefix 'main 'gtk-) 'gtk-main)
  (test/equal "stars" (name-symbol/prefix '*a-constant* 'gtk-) '*gtk-a-constant*)
  (test/equal "angle brackets" (name-symbol/prefix '<window> 'gtk-) '<gtk-window>))
