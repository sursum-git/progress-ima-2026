/* Programa: ESSP0199.P
** Modulo..: Controle de Qualidade
** Objetivo: Gerar Registro Totalizador na TEMP-TABLE do programa ESSP0199.W
** Autor...: FµBIO COELHO LANZA - FEVEREIRO/2010
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD dia          AS CHAR FORMAT "x(2)"
       FIELD mes          AS CHAR FORMAT "x(3)"
       FIELD cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
       FIELD producao     AS DEC
       FIELD perc-prod    AS DEC
       FIELD perfeito     AS DEC
       FIELD perc-perf    AS DEC
       FIELD regular      AS DEC
       FIELD perc-reg     AS DEC
       FIELD leve-def     AS DEC
       FIELD perc-ldef    AS DEC
       FIELD retalho      AS DEC
       FIELD perc-ret     AS DEC
       FIELD perc-tot     AS DEC
       FIELD perc-meta    AS DEC
       INDEX indice1 dia.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-work.
DEFINE INPUT-OUTPUT PARAMETER fi-producao AS DEC.
DEFINE INPUT        PARAMETER c-setor-def AS CHAR.

DEF BUFFER b-tt-work FOR tt-work.                                                         

DEF VAR de-producao  AS DEC.
DEF VAR de-perfeito  AS DEC.
DEF VAR de-regular   AS DEC.
DEF VAR de-leve-def  AS DEC.
DEF VAR de-retalho   AS DEC.

FOR EACH b-tt-work WHERE
                BREAK BY b-tt-work.dia.

    IF FIRST-OF(b-tt-work.dia) THEN
       ASSIGN de-perfeito   = 0
              de-regular    = 0
              de-leve-def   = 0
              de-retalho    = 0.

    ASSIGN de-perfeito   = de-perfeito + b-tt-work.perfeito
           de-regular    = de-regular  + b-tt-work.regular
           de-leve-def   = de-leve-def + b-tt-work.leve-def
           de-retalho    = de-retalho  + b-tt-work.retalho.

    IF LAST-OF(b-tt-work.dia) THEN DO:
       IF (de-perfeito + de-regular + de-leve-def + de-retalho) <> 0 THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.dia          = b-tt-work.dia 
                 tt-work.mes          = SUBSTR("JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ",INT(b-tt-work.dia) * 3 - 2,3)
                 tt-work.cod-tipo-def = c-setor-def
                 tt-work.producao     = b-tt-work.producao    
                 tt-work.perfeito     = b-tt-work.producao -
                                        de-regular -
                                        de-leve-def -
                                        de-retalho
                 tt-work.regular      = de-regular
                 tt-work.leve-def     = de-leve-def
                 tt-work.retalho      = de-retalho.
       END.
    END.
END.     

ASSIGN de-producao = 0.
FOR EACH b-tt-work WHERE
         b-tt-work.cod-tipo-def = c-setor-def NO-LOCK.
    ASSIGN de-producao = de-producao + b-tt-work.producao.
END.

FOR EACH b-tt-work WHERE
         b-tt-work.cod-tipo-def = c-setor-def NO-LOCK.

    FIND tipo-def WHERE
         tipo-def.cod-tipo-def = c-setor-def NO-LOCK NO-ERROR.

    ASSIGN b-tt-work.perc-prod = b-tt-work.producao / de-producao * 100
           b-tt-work.perc-perf = b-tt-work.perfeito / b-tt-work.producao * 100
           b-tt-work.perc-reg  = b-tt-work.regular / b-tt-work.producao * 100
           b-tt-work.perc-ldef = b-tt-work.leve-def / b-tt-work.producao * 100
           b-tt-work.perc-ret  = b-tt-work.retalho / b-tt-work.producao * 100
           b-tt-work.perc-tot  = (b-tt-work.regular + b-tt-work.leve-def + b-tt-work.retalho) /
                                 b-tt-work.producao * 100
           b-tt-work.perc-meta = tipo-def.metas.
END.
/* ASSIGN fi-producao = de-producao. */
