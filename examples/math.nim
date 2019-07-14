; rassoc 2 <=>
; macro (#a <=> #b) (macro #a #b)

; math #m <=> bash (bc <<< " #m ")

; even? #n <=> ::even? !(math (#n % 2))
; ::even? 0 <=> true
; ::even? 1 <=> false

; even? 33

; while #index #p #b <=>
   -while #index #p #b 0

; -while #index #p #b #c <=>
    (--while #index !(replace #p #index #c) #p #b #c)
; --while #index true #p #b #c <=>
    ( replace #b #index #c
    ; -while #index #p #b (!(math (#c + 1)))
    )
; --while #index false #p #b #c <=> done

; while <i> (even? <i>)
        (math (<i> * 2))
