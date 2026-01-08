/* Programa: ESCE0001.W
** Sistema.: EMS2.04 da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o Consumo de Itens do Beneficiamento
** Autor...: F bio Coelho Lanza - Abril/2005
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0001RP 2.04.00.000}

DEFINE TEMP-TABLE w-work
       FIELD it-codigo    like item.it-codigo
       FIELD descricao-1  LIKE ITEM.descricao-1
       FIELD descricao-2  LIKE ITEM.descricao-2
       FIELD per01-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per02-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per03-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per04-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per05-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per06-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per07-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per08-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per09-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per10-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per11-sai    as dec format ">>>,>>>,>>9.99"
       FIELD per12-sai    as dec format ">>>,>>>,>>9.99"
       INDEX ch-work it-codigo.

define temp-table tt-param no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       FIELD cod-estabel         LIKE saldo-estoq.cod-estabel
       FIELD ge-codigo-ini       LIKE item.ge-codigo         
       FIELD ge-codigo-fin       LIKE item.ge-codigo           
       FIELD ini-it-codigo       LIKE item.it-codigo      
       FIELD fin-it-codigo       LIKE item.it-codigo 
       FIELD ini-cod-obsoleto    LIKE item.cod-obsoleto     
       FIELD fin-cod-obsoleto    LIKE item.cod-obsoleto       
       FIELD fi-periodo-ini      AS CHAR FORMAT "x(7)"
       FIELD fi-periodo-fim      AS CHAR FORMAT "x(7)"
       FIELD c-per               AS CHAR FORMAT "x(8)" EXTENT 12
       FIELD all-depos           AS LOG FORMAT "Sim/NÆo"
       FIELD cod-depos1          LIKE deposito.cod-depos
       FIELD cod-depos2          LIKE deposito.cod-depos
       FIELD cod-depos3          LIKE deposito.cod-depos
       FIELD cod-depos4          LIKE deposito.cod-depos
       FIELD cod-depos5          LIKE deposito.cod-depos
       FIELD cod-depos6          LIKE deposito.cod-depos
       FIELD cod-depos7          LIKE deposito.cod-depos
       FIELD cod-depos8          LIKE deposito.cod-depos
       FIELD cod-depos9          LIKE deposito.cod-depos
       FIELD cod-depos10         LIKE deposito.cod-depos
       FIELD esp-docto1          LIKE movto-estoq.esp-docto
       FIELD esp-docto2          LIKE movto-estoq.esp-docto
       FIELD esp-docto3          LIKE movto-estoq.esp-docto
       FIELD esp-docto4          LIKE movto-estoq.esp-docto
       FIELD esp-docto5          LIKE movto-estoq.esp-docto
       FIELD esp-docto6          LIKE movto-estoq.esp-docto
       FIELD esp-docto7          LIKE movto-estoq.esp-docto
       FIELD esp-docto8          LIKE movto-estoq.esp-docto
       FIELD esp-docto9          LIKE movto-estoq.esp-docto
       FIELD esp-docto10         LIKE movto-estoq.esp-docto
       FIELD gerar-excel         AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel           AS CHAR FORMAT "x(45)"
       FIELD imp-param           AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR da-dt-ini AS DATE FORMAT "99/99/9999".
DEF VAR da-dt-fim AS DATE FORMAT "99/99/9999".
DEF VAR da-aux    AS DATE.
DEF VAR de-meses  AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12.
DEF VAR de-aux    AS DEC FORMAT "->>,>>>,>>9.99" EXTENT 12.
DEF VAR de-total  AS DEC FORMAT "->>,>>>,>>9.99".
DEF VAR i-cont    AS INT.
DEF VAR i-pto     AS INT.
DEF VAR i-ct      AS INT.
DEF VAR c-desc-esp-docto AS CHAR.
DEF VAR c-especies       AS CHAR.
DEF VAR c-descricao      AS CHAR FORMAT "x(36)".
DEF VAR c-item           AS CHAR FORMAT "x(55)".

DEF STREAM saida.

form
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.cod-estabel      label "Estabelecimento." AT  1
    tt-param.ge-codigo-ini    LABEL "Grupo Estoque..." AT  1
    "a"                                                AT 35
    tt-param.ge-codigo-fin    NO-LABEL                      
    tt-param.ini-it-codigo    label "Item............" AT  1
    "a"                                                AT 35                
    tt-param.fin-it-codigo    no-labels
    tt-param.ini-cod-obsoleto label "Situa‡Æo........" AT  1
    "a"                                                AT 35
    tt-param.fin-cod-obsoleto no-labels
    tt-param.fi-periodo-ini   label "Periodo........." AT  1
    "a"                                                AT 35                
    tt-param.fi-periodo-fim   no-labels
    tt-param.all-depos        LABEL "Todos Dep¢sitos." AT  1
    tt-param.cod-depos1       LABEL "Dep¢sitos......." AT  1
    tt-param.cod-depos2       NO-LABELS                AT 22
    tt-param.cod-depos3       NO-LABELS                AT 26
    tt-param.cod-depos4       NO-LABELS                AT 30
    tt-param.cod-depos5       NO-LABELS                AT 34
    tt-param.cod-depos6       NO-LABELS                AT 38
    tt-param.cod-depos7       NO-LABELS                AT 42
    tt-param.cod-depos8       NO-LABELS                AT 46
    tt-param.cod-depos9       NO-LABELS                AT 50
    tt-param.cod-depos10      NO-LABELS                AT 54
    c-desc-esp-docto          LABEL "Esp‚cies Docto.." FORMAT "x(50)" AT 1 
    tt-param.gerar-excel      LABEL "Gerar Excel....." AT  1
    tt-param.arq-excel        LABEL "Arquivo Excel..." AT  1
    with no-box side-labels width 132 stream-io frame f-param.

form
    w-work.it-codigo FORMAT "x(6)"     
    w-work.descricao-1      
    de-meses[ 1]
    de-meses[ 2]
    de-meses[ 3]
    de-meses[ 4]
    de-meses[ 5]
    de-meses[ 6] 
    de-total SKIP
    w-work.descricao-2 AT  8
    de-meses[ 7]       AT 27
    de-meses[ 8]
    de-meses[ 9]
    de-meses[10]
    de-meses[11]
    de-meses[12]
    
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

form header
    tt-param.c-per[ 1]  at  33
    tt-param.c-per[ 2]  at  48
    tt-param.c-per[ 3]  at  63
    tt-param.c-per[ 4]  at  78
    tt-param.c-per[ 5]  at  93
    tt-param.c-per[ 6]  at 108
    "TOTAL"             at 126
    "ITEM   DESCRICAO"  at   1
    tt-param.c-per[ 7]  at  33
    tt-param.c-per[ 8]  at  48
    tt-param.c-per[ 9]  at  63
    tt-param.c-per[10]  at  78
    tt-param.c-per[11]  at  93
    tt-param.c-per[12]  at 108
    "MEDIA"             at 126             
    "------ ------------------ -------------- -------------- -------------- --------------"  at   1
    " -------------- -------------- -------------- " at  86
    with width 132 no-labels no-box page-top STREAM-IO frame f-cabecalho.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Consumo_de_Materiais * r}
assign c-titulo-relat = trim(return-value).

/* Formata o Data Inicial/Final a partir do Periodo AAAAMM */
ASSIGN da-dt-ini = DATE(INT(SUBSTR(tt-param.fi-periodo-ini,5,2)), 1,INT(SUBSTR(tt-param.fi-periodo-ini,1,4))).
       da-aux    = DATE(INT(SUBSTR(tt-param.fi-periodo-fim,5,2)),28,INT(SUBSTR(tt-param.fi-periodo-fim,1,4))) + 4.
       da-dt-fim = DATE(MONTH(da-aux),1,YEAR(da-aux)) - 1.

view frame f-cabec.
view frame f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   output STREAM saida to value(tt-param.arq-excel) CONVERT SOURCE "ibm850".
   PUT STREAM saida 
       "ITEM;"
       "DESCRICAO;" 
       "UN;"
       tt-param.c-per[1] ";"
       tt-param.c-per[2] ";"  
       tt-param.c-per[3] ";"  
       tt-param.c-per[4] ";"  
       tt-param.c-per[5] ";"  
       tt-param.c-per[6] ";"  
       tt-param.c-per[7] ";"  
       tt-param.c-per[8] ";"  
       tt-param.c-per[9] ";"  
       tt-param.c-per[10] ";"  
       tt-param.c-per[11] ";"  
       tt-param.c-per[12] ";"
       "TOTAL;"             
       "MEDIA"                          
       SKIP.
END.

FOR EACH  movto-estoq USE-INDEX data-item
    WHERE movto-estoq.dt-trans  >= da-dt-ini
      AND movto-estoq.dt-trans  <= da-dt-fim
      AND movto-estoq.it-codigo >= tt-param.ini-it-codigo
      AND movto-estoq.it-codigo <= tt-param.fin-it-codigo
    NO-LOCK:

    run pi-acompanhar in h-acomp (input "Data: "  + string(movto-estoq.dt-trans) +
                                        " Item: " + movto-estoq.it-codigo).
    IF movto-estoq.cod-estabel <> tt-param.cod-estabel THEN NEXT.

    if tt-param.all-depos =  no and 
       movto-estoq.cod-depos <> tt-param.cod-depos1 and 
       movto-estoq.cod-depos <> tt-param.cod-depos2 and  
       movto-estoq.cod-depos <> tt-param.cod-depos3 and 
       movto-estoq.cod-depos <> tt-param.cod-depos4 and 
       movto-estoq.cod-depos <> tt-param.cod-depos5 and 
       movto-estoq.cod-depos <> tt-param.cod-depos6 and 
       movto-estoq.cod-depos <> tt-param.cod-depos7 and 
       movto-estoq.cod-depos <> tt-param.cod-depos8 and 
       movto-estoq.cod-depos <> tt-param.cod-depos9 and 
       movto-estoq.cod-depos <> tt-param.cod-depos10 THEN NEXT.

    if movto-estoq.esp-docto <> tt-param.esp-docto1 and 
       movto-estoq.esp-docto <> tt-param.esp-docto2 and 
       movto-estoq.esp-docto <> tt-param.esp-docto3 and 
       movto-estoq.esp-docto <> tt-param.esp-docto4 and 
       movto-estoq.esp-docto <> tt-param.esp-docto5 and 
       movto-estoq.esp-docto <> tt-param.esp-docto6 and 
       movto-estoq.esp-docto <> tt-param.esp-docto7 and 
       movto-estoq.esp-docto <> tt-param.esp-docto8 and 
       movto-estoq.esp-docto <> tt-param.esp-docto9 and 
       movto-estoq.esp-docto <> tt-param.esp-docto10 then NEXT.

    FIND item WHERE item.it-codigo     = movto-estoq.it-codigo
                AND ITEM.ge-codigo    >= tt-param.ge-codigo-ini
                AND ITEM.ge-codigo    <= tt-param.ge-codigo-fin
                and item.cod-obsoleto >= tt-param.ini-cod-obsoleto
                and item.cod-obsoleto <= tt-param.fin-cod-obsoleto
              NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    find first w-work where w-work.it-codigo = movto-estoq.it-codigo
                      no-lock no-error.
    if not avail w-work then do:
       create w-work.
       assign w-work.it-codigo   = movto-estoq.it-codigo
              w-work.descricao-1 = TRIM(ITEM.DESCRICAO-1)
              w-work.descricao-2 = TRIM(ITEM.descricao-2)
              w-work.per01-sai   = 0
              w-work.per02-sai   = 0
              w-work.per03-sai   = 0
              w-work.per04-sai   = 0
              w-work.per05-sai   = 0
              w-work.per06-sai   = 0
              w-work.per07-sai   = 0
              w-work.per08-sai   = 0
              w-work.per09-sai   = 0
              w-work.per10-sai   = 0
              w-work.per11-sai   = 0
              w-work.per12-sai   = 0.
    end.
    CASE MONTH(movto-estoq.dt-trans).
         WHEN 1  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per01-sai = w-work.per01-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per01-sai = w-work.per01-sai - movto-estoq.quantidade.
         WHEN 2  then
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per02-sai = w-work.per02-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per02-sai = w-work.per02-sai - movto-estoq.quantidade.
         WHEN 3  then
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per03-sai = w-work.per03-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per03-sai = w-work.per03-sai - movto-estoq.quantidade.
         WHEN 4  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per04-sai = w-work.per04-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per04-sai = w-work.per04-sai - movto-estoq.quantidade.
         WHEN 5  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per05-sai = w-work.per05-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per05-sai = w-work.per05-sai - movto-estoq.quantidade.
         WHEN 6  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per06-sai = w-work.per06-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per06-sai = w-work.per06-sai - movto-estoq.quantidade.
         WHEN 7  then
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per07-sai = w-work.per07-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per07-sai = w-work.per07-sai - movto-estoq.quantidade.
         WHEN 8  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per08-sai = w-work.per08-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per08-sai = w-work.per08-sai - movto-estoq.quantidade.
         WHEN 9  then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per09-sai = w-work.per09-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per09-sai = w-work.per09-sai - movto-estoq.quantidade.
         WHEN 10 then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per10-sai = w-work.per10-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per10-sai = w-work.per10-sai - movto-estoq.quantidade.
         WHEN 11 then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per11-sai = w-work.per11-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per11-sai = w-work.per11-sai - movto-estoq.quantidade.
         WHEN 12 then 
              IF movto-estoq.tipo-trans = 2 THEN
                 assign w-work.per12-sai = w-work.per12-sai + movto-estoq.quantidade.
              ELSE
                 assign w-work.per12-sai = w-work.per12-sai - movto-estoq.quantidade.
    END CASE.
end.

for each w-work break BY IF tt-param.classific = 1 THEN w-work.it-codigo
                         ELSE w-work.descricao-1 + w-work.descricao-2:
    ASSIGN de-aux[ 1] = w-work.per01-sai
           de-aux[ 2] = w-work.per02-sai
           de-aux[ 3] = w-work.per03-sai
           de-aux[ 4] = w-work.per04-sai
           de-aux[ 5] = w-work.per05-sai
           de-aux[ 6] = w-work.per06-sai
           de-aux[ 7] = w-work.per07-sai
           de-aux[ 8] = w-work.per08-sai
           de-aux[ 9] = w-work.per09-sai
           de-aux[10] = w-work.per10-sai
           de-aux[11] = w-work.per11-sai
           de-aux[12] = w-work.per12-sai
           i-pto      = INT(SUBSTR(tt-param.fi-periodo-ini,5,2))
           de-total   = 0
           de-meses   = 0
           de-total   = de-total + de-meses[1 ].
   
    IF i-pto > 1 THEN DO:
       ASSIGN i-ct = 1.
       DO i-cont = i-pto   TO 12:
          ASSIGN de-meses[i-ct] = de-aux[i-cont]
                 de-total       = de-total + de-aux[i-cont]
                 i-ct           = i-ct + 1.
       END.
       DO  i-cont = 1 TO i-pto - 1:
           ASSIGN de-meses[i-ct] = de-aux[i-cont]
                  de-total       = de-total + de-aux[i-cont]
                  i-ct           = i-ct + 1.
       END.
    END.
    ELSE
       DO i-cont = 1 TO 12:
          ASSIGN de-meses[i-cont] = de-aux[i-cont]
                 de-total       = de-total + de-aux[i-cont].
       END.

    IF de-total <> 0  THEN DO:
       display w-work.it-codigo
               w-work.descricao-1
               de-meses[ 1]
               de-meses[ 2]    
               de-meses[ 3]      
               de-meses[ 4]        
               de-meses[ 5]          
               de-meses[ 6]
               w-work.descricao-2
               de-meses[ 7]
               de-meses[ 8]    
               de-meses[ 9]      
               de-meses[10]        
               de-meses[11]          
               de-meses[12]
               de-total
               de-total / 12 FORMAT "->>,>>>,>>9.99" SKIP(1)
               with frame f-detalhe.
       down with frame f-detalhe.

       IF tt-param.gerar-excel THEN DO:
          FIND ITEM WHERE ITEM.it-codigo = w-work.it-codigo NO-LOCK.
          PUT STREAM saida 
                     ITEM.it-codigo ";"
                     item.desc-item ";"
                     ITEM.un ";"
                     de-meses[1] ";"
                     de-meses[2] ";"    
                     de-meses[3] ";"      
                     de-meses[4] ";"        
                     de-meses[5] ";"          
                     de-meses[6] ";"
                     de-meses[7] ";"
                     de-meses[8] ";"    
                     de-meses[9] ";"      
                     de-meses[10] ";"        
                     de-meses[11] ";"          
                     de-meses[12] ";"
                     de-total ";"
                     de-total / 12
                     SKIP.
       END.
    END.
end.
 
IF tt-param.gerar-excel THEN
   OUTPUT STREAM saida CLOSE.

IF tt-param.imp-param THEN DO:

    {esinc/i-dsallrb.i movto-estoq.esp-docto c-especies}

    ASSIGN c-desc-esp-docto = "".
    IF tt-param.esp-docto1 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + entry(tt-param.esp-docto1,c-especies).
    IF tt-param.esp-docto2 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto2,c-especies).
    IF tt-param.esp-docto3 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto3,c-especies).
    IF tt-param.esp-docto4 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto4,c-especies).
    IF tt-param.esp-docto5 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto5,c-especies).
    IF tt-param.esp-docto6 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto6,c-especies).
    IF tt-param.esp-docto7 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto7,c-especies).
    IF tt-param.esp-docto8 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto8,c-especies).
    IF tt-param.esp-docto9 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto9,c-especies).
    IF tt-param.esp-docto10 > 0 THEN
       ASSIGN c-desc-esp-docto = c-desc-esp-docto + ", " + entry(tt-param.esp-docto10,c-especies).
   PAGE.
   display tt-param.cod-estabel
           tt-param.ge-codigo-ini  
           tt-param.ge-codigo-fin  
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           tt-param.ini-cod-obsoleto     
           tt-param.fin-cod-obsoleto     
           tt-param.fi-periodo-ini
           tt-param.fi-periodo-fim
           tt-param.all-depos
           tt-param.cod-depos1
           tt-param.cod-depos2
           tt-param.cod-depos3
           tt-param.cod-depos4
           tt-param.cod-depos5
           tt-param.cod-depos6
           tt-param.cod-depos7
           tt-param.cod-depos8
           tt-param.cod-depos9
           tt-param.cod-depos10
           c-desc-esp-docto
           tt-param.gerar-excel 
           tt-param.arq-excel   
           with frame f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

