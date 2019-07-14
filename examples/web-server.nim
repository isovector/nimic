{
  macro (web #resp) (bash (echo -e 'HTTP/1.1 200 OK\r\n #resp \n' | nc -l 9090))

; (web "yo")
}