/* Programa: imp-fardos.p
** Objetivo: Importar o estoque de fardos de algod∆o existentes antes da
**           implantaá∆o dos sistema de etiquetamento.
*/

DEF BUFFER b-mp-entr-mat FOR mp-entr-mat.
                      
DEF VAR de-peso       LIKE mp-entr-mat.peso-nf.
DEF VAR i-nr-cdr      LIKE mp-entr-mat.nr-cdr.
DEF VAR i-nr-fardo    LIKE mp-fardo.nr-fardo.
DEF VAR i-qtd-fardos  AS INT.
DEF VAR i-registro    AS RECID.
DEF VAR i-cont        AS INT.
DEF VAR l-erro        AS LOG.

def temp-table tt-work
    FIELD codigo       AS char
    FIELD fornecedor   AS CHAR
    FIELD nota-fiscal  AS CHAR
    FIELD data-receb   AS CHAR
    FIELD procedencia  AS char
    FIELD padrao       AS CHAR
    FIELD cd-tipo      AS CHAR
    FIELD tipo         AS CHAR
    FIELD cd-coloracao AS CHAR
    FIELD tonalidade   AS CHAR
    FIELD cd-compr     AS CHAR
    FIELD letra        AS CHAR
    FIELD comprimento  AS CHAR
    FIELD peso         AS char 
    FIELD finura       AS CHAR 
    FIELD resistencia  AS CHAR 
    FIELD maturidade   AS CHAR 
    FIELD sl-1         AS char 
    FIELD sl-2         AS CHAR 
    FIELD ur           AS CHAR 
    FIELD desperdicio  AS CHAR
    FIELD deposito     AS CHAR 
    FIELD localizacao  AS CHAR.
    
input from "especificos/fardos.csv".
SET ^.

repeat:
   create tt-work.
   import delimiter ";" tt-work.
end.
input close.

/*--- Consistencia dos dados da planilha ---*/
OUTPUT TO "c:/temp/err_imp-fardos.txt".
FOR EACH tt-work:
    FIND mp-entr-mat WHERE mp-entr-mat.cod-emit  = INT(tt-work.codigo)     
                       AND mp-entr-mat.nro-docto = INT(tt-work.nota-fiscal)
                     NO-LOCK NO-ERROR.
         IF AVAIL mp-entr-mat THEN DO:
            ASSIGN i-cont = i-cont + 1
                   l-erro = YES.
            PUT "Linha: " i-cont
                " - Fornecedor/Documento: " 
                tt-work.codigo " / " 
                tt-work.nota-fiscal
                " ja existe no MP-ENTR-MAT."
                SKIP.
         END.
END.

IF l-erro THEN
   MESSAGE "H† erros de consistància. Veja arquivo C:/Temp/err_imp-fardos.txt."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:
   FOR EACH tt-work WHERE tt-work.codigo <> ""
                    BREAK BY tt-work.fornecedor
                          BY tt-work.nota-fiscal:
   
       IF FIRST-OF(tt-work.nota-fiscal) THEN DO:
          FIND LAST b-mp-entr-mat USE-INDEX indice1 NO-LOCK NO-ERROR.
          IF AVAIL b-mp-entr-mat THEN
             ASSIGN i-nr-cdr = b-mp-entr-mat.nr-cdr + 1.
          ELSE
             ASSIGN i-nr-cdr = 1.
          CREATE mp-entr-mat.
          ASSIGN mp-entr-mat.nr-cdr         = i-nr-cdr
                 mp-entr-mat.procedencia    = tt-work.procedencia
                 mp-entr-mat.cod-emit       = INT(tt-work.codigo)
                 mp-entr-mat.nro-docto      = INT(tt-work.nota-fiscal)
                 mp-entr-mat.disperdicio    = DEC(tt-work.desperdicio)
                 mp-entr-mat.dt-emissao-nf  = DATE(INT(SUBSTR(tt-work.data-receb,4,2)),
                                                   INT(SUBSTR(tt-work.data-receb,1,2)),
                                                   INT(SUBSTR(tt-work.data-receb,7,4)))
                 mp-entr-mat.dt-recebimento = DATE(INT(SUBSTR(tt-work.data-receb,4,2)), 
                                                   INT(SUBSTR(tt-work.data-receb,1,2)), 
                                                   INT(SUBSTR(tt-work.data-receb,7,4)))
                 mp-entr-mat.padrao[1]      = tt-work.padrao.
          ASSIGN i-registro = RECID(mp-entr-mat).
       END.
   
       /*--- Cria Fardos ---*/
       FIND mp-entr-mat WHERE RECID(mp-entr-mat) = i-registro.
       ASSIGN i-nr-fardo = INT(SUBSTR(STRING(mp-entr-mat.dt-recebimento,"999999"),3,4) + '0001').
       FIND FIRST mp-fardo WHERE
                  mp-fardo.nr-fardo = i-nr-fardo NO-LOCK NO-ERROR.
   
       IF NOT AVAIL mp-fardo THEN
          CURRENT-VALUE(seq-fardo) = i-nr-fardo - 1.  /* No primeiro fardo do Mes/Ano, zera sequencial */
                                                      /* para o Next-Value n∆o pular o 0001 */
       CREATE mp-fardo.
       ASSIGN mp-fardo.nr-cdr       = i-nr-cdr
              mp-fardo.nr-fardo     = NEXT-VALUE(seq-fardo)
              mp-fardo.padrao       = tt-work.padrao
              mp-fardo.letra        = tt-work.letra
              mp-fardo.cd-tipo      = INT(tt-work.cd-tipo)
              mp-fardo.cd-coloracao = INT(tt-work.cd-coloracao)
              mp-fardo.cd-compr     = INT(tt-work.cd-compr)
              mp-fardo.sl1          = INT(tt-work.sl-1)
              mp-fardo.sl2          = INT(tt-work.sl-2)
              mp-fardo.ur           = INT(tt-work.ur)
              mp-fardo.peso         = INT(tt-work.peso)
              mp-fardo.cod-depos    = tt-work.deposito
              mp-fardo.cod-localiz  = tt-work.localizacao
              mp-fardo.situacao     = 3 /* Em estoque */
              mp-fardo.finura       = INT(tt-work.finura)
              mp-fardo.resistencia  = INT(tt-work.resistencia)
              mp-fardo.maturidade   = INT(tt-work.maturidade).
   
       ASSIGN i-qtd-fardos = i-qtd-fardos + 1.
              de-peso      = de-peso + DEC(tt-work.peso).
       IF LAST-OF(tt-work.nota-fiscal) THEN DO:
          FIND mp-entr-mat WHERE RECID(mp-entr-mat) = i-registro.
          ASSIGN mp-entr-mat.peso-nf       = de-peso
                 mp-entr-mat.qtd-fardos[1] = i-qtd-fardos.
   
          ASSIGN de-peso      = 0.
                 i-qtd-fardos = 0.
       END.
   END.
END.

OUTPUT CLOSE.
