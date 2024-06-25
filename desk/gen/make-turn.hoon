:-  %say  |=  *  :-  %noun
::
!.
=>  0
=>  |%
    ++  list
      |$  [item]
      $@(~ [i=item t=(list item)])
    ::
    ++  gulf
      |=  [a=@ b=@]
      |-  ^-  (list @)
      ?:(=(a +(b)) ~ [a $(a +(a))])
    --
|^
|.(~:(turn-atoms ^~((gulf 190 200)) dec))
::
++  turn-atoms
  |=  [a=(list @) b=$-(@ @)]
  ^-  (list @)
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  dec
  |=  a=@
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
--