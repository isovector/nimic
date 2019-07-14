; rassoc 3 :
; rassoc 4 ->
; rassoc 3 =

; #t is a type
  ----------
  #a : #t
  ----------
  typeof #a
  ----------
  #t

; Bool is a type
; True  : Bool
; False : Bool

; [#a] is a type
; [] : [#a]

; #a = #b
  ----------
  #a
  ----------
  #b

; not #t = match #t of
             ( True -> False
             ; False -> True
             )

; match (not True) of
    ( True -> hello
    ; False -> world
    )

