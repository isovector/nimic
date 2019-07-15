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
  (socket-resp !(bash (echo #msg | nc -l #port)))

; while i
        true
        (write-read-socket ("the i 'th test") 9090)
