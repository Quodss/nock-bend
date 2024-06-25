/+  *bend-tools
::
:-  %say  |=  *  :-  %noun
::
=;  add
  (bend-noun add)
!.  =>  ~
|=  [a=@ b=@]
^-  @
?:  =(a 0)  b
?:  =(b 0)  a
=+  c=0
|-  ^-  @
?:  =(a c)  b
$(b +(b), c +(c))