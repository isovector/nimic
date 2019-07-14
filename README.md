- make force be steppable 
- if you ommit a paran, the parser sometimes drops a term ie.
; even? #n <=> ::even? !(math (#n % 2)
will parse to
(even? #n <=>)

```
(if true then [b] else [c]); [rest]
-----------------------------------
[b]; [rest]
```


```
if false then [b] else [c]; [rest]
-------------------------
[c]; [rest]
```


```
if [a] then [b] else [c]; [rest]
-------------------------
[a]; if [a] then [b] else [c]; [rest]
```


```
not true; [a]
-------------
false; [a]
```


```
not false; [a]
-------------
true; [a]
```


```
not [a]; [b]
-------------
[a]; not [a]; [b]
```


```
true; [a]
-------------
[a]
```


```
false; [a]
-------------
[a]
```


```
[a] is a value
--------------
[a]: [rest]
--------------
[rest]
```


```
nil is a value
cons is a value
```


```
let [a] = [b] in [c]
--------------------
<foo [a] [b]>  [c]
```


```
<foo [a] [b]>  var [a]
----------------------
[b]
```


```
----------------------
primitive printLine [msg]
```


```
emit [a]; [rest]
------------
[rest]; [a]
```






```
if (not (not false)) then 1 else 2
not (not false); if (not (not (false))) then 1 else 2
not false; not (not false); if (not (not (false))) then 1 else 2
true; not true; if (not true) then 1 else 2
not true; if (not true) then 1 else 2
false; if false then 1 else 2
if false then 1 else 2
2
```

