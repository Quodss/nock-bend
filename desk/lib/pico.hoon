=>  %pc0
|%
++  dec
  |=  a=@
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  add
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  sub
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
++  mul
  |:  [a=`@`1 b=`@`1]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  dvr
  |:  [a=`@`1 b=`@`1]
  ^-  [p=@ q=@]
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  [c a]
  $(a (sub a b), c +(c))
::
++  div
  |:  [a=`@`1 b=`@`1]
  ^-  @
  -:(dvr a b)
::
++  mod
  |:  [a=`@`1 b=`@`1]
  ^-  @
  +:(dvr a b)
::
--