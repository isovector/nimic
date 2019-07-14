(macro (#a ; #b) #b)
; ((rassoc 1 ;)
; macro
  ( #a
    ----------
    #b
  )
  (macro #a #b)

; rassoc 2 ----------
; rassoc 3 //

; import #file
  ----------
  bash (cat #file)

; import examples/math.nim
))
