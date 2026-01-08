DEF VAR c-programas AS CHAR.

IF PROGRAM-NAME(1) <> ? THEN
   ASSIGN c-programas = PROGRAM-NAME(1) + CHR(10).
IF PROGRAM-NAME(2) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(2) + CHR(10). 
IF PROGRAM-NAME(3) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(3) + CHR(10).
IF PROGRAM-NAME(4) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(4) + CHR(10).
IF PROGRAM-NAME(5) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(5) + CHR(10).
IF PROGRAM-NAME(6) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(6) + CHR(10).
IF PROGRAM-NAME(7) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(7) + CHR(10).
IF PROGRAM-NAME(8) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(8) + CHR(10).
IF PROGRAM-NAME(9) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(9) + CHR(10).
IF PROGRAM-NAME(10) <> ? THEN
   ASSIGN c-programas = c-programas + PROGRAM-NAME(10) + CHR(10).

MESSAGE c-programas
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
