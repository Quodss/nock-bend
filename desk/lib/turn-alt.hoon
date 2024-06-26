/+  bend
=>  bend
|%
++  list
  |$  [item]
  $@(~ [i=item t=(list item)])
::
++  snag
  |*  [a=@ b=(list)]
  |-  ^+  ?>(?=(^ b) i.b)
  ?~  b  !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  homo
  |*  a=(list)
  ^+  =<  $
    |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  lent
  |=  a=(list)
  ^-  @
  =+  b=0
  |-
  ?~  a  b
  $(a t.a, b +(b))
::
++  turn-std
  |*  [a=(list) b=gate]
  =>  .(a (homo a))
  ^-  (list _?>(?=(^ a) (b i.a)))
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  turn-alt
  |*  [a=(list) b=gate]
  =>  .(a (homo a))
  =/  len-a=@  (lent a)
  |-  ^-  (list _?>(?=(^ a) (b i.a)))
  =*  split  $
  ?:  =(len-a 0)  ~
  ?:  =(len-a 1)  ~[(b -.a)]
  =/  len-half=@  (div len-a 2)
  =/  c  `_a`~
  =/  len-c=@  0
  |-  ^-  (list _?>(?=(^ a) (b i.a)))
  =*  pop  $
  ?.  =(len-c len-half)
    pop(len-c +(len-c), c [-.a c], a +.a)
  =+  ^=  [c1 a1]
    :-  split(a c, len-a len-c)  ::  hopefully it will help to parallelize turn
    split(len-a (sub len-a len-half))
  |-  ^-  (list _?>(?=(^ a) (b i.a)))
  =*  push  $
  ?~  c1  a1
  $(c1 t.c1, a1 [i.c1 a1])
--