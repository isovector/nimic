; rassoc 4 ->
; rassoc 4 =

; // lambda elimination
; (λ #x -> #f) #a
  ----------
  !(replace #f #x #a)
; (λ #x #y -> #f) #a #b
  ----------
  !(replace !(replace #f #y !#b) #x !#a)
; (λ #x #y #z -> #f) #a #b #c
  ----------
  !(replace !(replace !(replace #f #z !#c) #y !#b) #x !#a)
; (λ #x #y #z #w -> #f) #a #b #c #d
  ----------
  !(replace !(replace !(replace !(replace #f #w !#d) #z !#c) #y !#b) #x !#a)

; // function introduction
; #fn #x = #body
  ----------
  #fn
  ----------
  (λ #x -> #body)
; #fn #x #y = #body
  ----------
  #fn
  ----------
  (λ #x #y -> #body)
; #fn #x #y #z = #body
  ----------
  #fn
  ----------
  (λ #x #y #z -> #body)
; #fn #x #y #z #w = #body
  ----------
  #fn
  ----------
  (λ #x #y #z #w -> #body)

