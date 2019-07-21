(macro (#a ; #b) #b)
; ( (rassoc 1 ;)
  ; macro
      (
        #a
        ----------
        #b
      )
      (macro #a #b)
  ; rassoc 2 ----------
  ; rassoc 3 //
  ; // we now have comments

  ; import #module
    ----------
    bash (cat #module)

  ; // another syntax for macros
  ; rassoc 2 => ;
  ; macro (#a => #b) (macro #a #b)

  ; import examples/math.nim
  )
