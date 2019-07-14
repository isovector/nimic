; rassoc 2 ----------
; #a concat #b
  ----------
  bash (echo " #a #b " | awk ' { print $1$2 } ')

; #n repeat #c
  ----------
  (!(math (#n - 1)) repeat #c) concat #c

; (1) repeat #c
  ----------
  #c

; sequent #n
  ----------
  ( #a !(#n repeat -) #b
    ----------
    #a
    ----------
    #b
  ; rassoc 2 !(#n repeat -)
  )

; sequent 9
; sequent 8
; sequent 7
; sequent 6
; sequent 5
; sequent 4
; sequent 3
