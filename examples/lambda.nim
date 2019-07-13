{ macro { #a
          ----------
          #b

        ; #r
        }

        { macro #a #b
        ; #r
        }

; (#a #b)
  ----------
  (!#a #b)

; ((λ #a . #c) #b)
  ----------
  (replace #c #a #b)

; ((λ y . (λ x . (x + y))) 5) 6
}
