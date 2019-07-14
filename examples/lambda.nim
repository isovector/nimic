; import examples/math.nim
; rassoc 2 ----------
; fix #a
  ----------
  #a (fix #a)

; let (#a = #b)
  ----------
  #a
  ----------
  #b
; let (hello = (math (1 + 5)))
; // hello world this is sandy speaking
; hello
; done
