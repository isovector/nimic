(rassoc 1 ;)
; ( macro (#a ; #b) #b
; macro
  ( #a
    ----------
    #b
  )
  (macro #a #b)

; rassoc 3 //

; (import #file)
  ----------
  (bash (cat #file))
; // a macro won't get forced if it's the last statement
)

