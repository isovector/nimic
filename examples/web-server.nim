; (http-200)
  ----------
  ('HTTP/1.1 200 OK\r\n')

; (web #resp)
  ----------
  (bash (echo !(http-200) ' #resp ' | nc -l 9090)
  ; web #resp
  )

; web yo