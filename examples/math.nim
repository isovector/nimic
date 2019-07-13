{
  macro (math #m) (bash (bc <<< " #m "));
  macro (web #resp) (bash (echo 'HTTP/1.1 200 OK\r\n #resp ' | nc -l 9090));
  (web "yo")
}



