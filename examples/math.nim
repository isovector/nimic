; math #m => bash (bc <<< " #m ")

; assert-false #v => assert-false !#v
; assert-true #v => assert-true !#v

; assert-false false => ()
; assert-true true => ()

; zero? #n => false
; zero? 0 => true

; not true => false
; not false => true

; #a < #b => not !(zero? !(math (#a < #b)))

; even? #n => -even? !(math (!#n % 2))
; -even? 0 => true
; -even? 1 => false

; assert-false (even? 33)

; inc #i => math (#i + 1)

; while #i #p #b => -while #i #p #b 0
; -while #i #p #b #c => --while #i !(replace #p #i #c) #p #b #c
; --while #i true #p #b #c =>
    ( replace #b #i #c
    ; -while #i #p #b !(inc #c)
    )
; --while #i false #p #b #c => ()

; while <i>
        (<i> < 2)
        (assert-true (even? (math (<i> * 2))))
