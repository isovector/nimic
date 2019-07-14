macro (math #m ; #r) (bash (bc <<< " #m ") ; #r)
