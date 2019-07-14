{ macro
  { #a
    ----------
    #b

  ; #r
  }

  { macro #a #b
  ; #r
  }

; { // #a ; #r }
  ----------
  #r

; { (#a) ; #b }
  ----------
  { #a ; #b }

; { {#a ; #b} ; #c }
  ----------
  { #a ; #b ; #c }

; { (bash #foo); #r }
  ----------
  { !(bash #foo); #r }

; { (import #file); #r }
  ----------
  { bash (cat #file); #r }

; // (a macro won't get forced if it's the last statement)
}

