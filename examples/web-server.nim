; (http-200)
  ----------
  ('HTTP/1.1 200 OK\r\n')

; (web #resp)
  ----------
  (bash (echo !(http-200) ' #resp ' | nc -l 9090)
  ; web #resp
  )

; (write-read-socket #msg #port)
  ----------
  (bash (echo #msg | nc -l #port))

; (read-socket #port)
  ----------
  (bash (nc -l #port))

; write-read-socket ("this is a test") 9090

