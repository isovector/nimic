; import examples/math.nim
; rassoc 2 ----------
; fix #a
  ----------
  #a (fix #a)

; (let (#a = #b) ; #rest)
  ----------
  ( ( #a ; #rest2)
    ----------
    ( #b ; #rest2)
  ; #a
    ----------
    #b
  ; #rest
  )
; let (hello = (math (1 + 5)))
; // hello world this is sandy speaking
; hello
; done
