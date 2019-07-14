; judge #a #b
  ----------
  is zero !(bash ([[ " !#a " == " !#b " ]]; echo $?))

; is zero #n
  ----------
  False

; is zero 0
  ----------
  True

; if #cond then #a else #b
  ----------
  if !#cond then #a else #b

; if True then #a else #b
  ----------
  #a

; if False then #a else #b
  ----------
  #b

; match #a of (#m1 -> #r1)
  ----------
  if !(judge #a #m1) then #r1 else (failed match)

; match #a of (#m1 -> #r1 ; #z)
  ----------
  if !(judge #a #m1) then #r1 else (match !#a of #z)

