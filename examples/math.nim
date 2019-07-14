macro (math #m) (!(bash (bc <<< " !#m ")))
