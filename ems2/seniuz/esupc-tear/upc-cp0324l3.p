/* Programa: upc-cp0324l3.p
** Objetivo: 
** Autor...: SeniuZ - Toninho  Mar‡o/2004
*/

DEF NEW GLOBAL SHARED VAR h-it-codigo AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-dep-pad-sai AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cod-depos  AS HANDLE.

FIND item WHERE 
     item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.

IF item.ge-codigo = 21 OR
   item.ge-codigo = 22 THEN DO.
   ASSIGN h-dep-pad-sai:SCREEN-VALUE = 'PRE'
          h-cod-depos:SCREEN-VALUE = 'ENG'.
   ASSIGN h-cod-depos:SENSITIVE = NO.
END.

IF item.ge-codigo = 40 OR
   item.ge-codigo = 41 THEN DO.
   ASSIGN h-dep-pad-sai:SCREEN-VALUE = 'ENG'
          h-cod-depos:SCREEN-VALUE = 'ENG'.
   ASSIGN h-cod-depos:SENSITIVE = NO.
END.

IF item.ge-codigo = 50 THEN DO.
   ASSIGN h-dep-pad-sai:SCREEN-VALUE = 'ENG'
          h-cod-depos:SCREEN-VALUE = 'TEC'.
   ASSIGN h-cod-depos:SENSITIVE = NO.
END.

