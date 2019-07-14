{ macro { #a
          ----------
          #b

        ; #r
        }

        { macro #a #b
        ; #r
        }

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

; import examples/math.nim

; math (5 + 6)

; done

}
