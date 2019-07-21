; rassoc 3 math
; math #m => bash (bc <<< " #m ")

; even? #n => -even? !(math #n % 2)
; -even? 0 => true
; -even? 1 => false

; even? 33

; while #i #p #b => -while #i #p #b 0

; -while #i #p #b #c => --while #i !(replace #p #i #c) #p #b #c
; --while #i true #p #b #c =>
    ( replace #b #i #c
    ; -while #i #p #b (!(math #c + 1))
    )
; --while #i false #p #b #c => done

; while <i>
        (even? <i>)
        (math <i> * 2)
