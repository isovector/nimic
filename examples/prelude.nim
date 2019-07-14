(rassoc 1 ;)
;
( macro
  ( #a
    ----------
    #b
  ; #r
  )

  ( macro #a #b
  ; #r
  )

; rassoc 2 ----------

; ( // #a ; #r )
  ----------
  #r

; ((discard #noisy-macro); #r)
  ----------
  (// !#noisy-macro ; #r)

; ( (#a) ; #b )
  ----------
  ( #a ; #b )

; ( (#a ; #b) ; #c )
  ----------
  ( #a ; #b ; #c )

; ( (bash #foo); #r )
  ----------
  ( !(bash #foo); #r )

; ( (import #file); #r )
  ----------
  ( bash (cat #file); #r )

; ( ; #r )
  ----------
  #r

; // (a macro won't get forced if it's the last statement)
)

