DEF VAR vl-maior-tit LIKE titulo.vl-original.
DEF VAR dt-maior-tit LIKE titulo.dt-emissao.
DEF VAR vl-ult-tit   LIKE titulo.vl-original.
DEF VAR dt-ult-tit   LIKE titulo.dt-emissao.

DEF VAR l-erro AS LOG.

def var h-acomp as handle no-undo.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".
PUT "ATUALIZA€ÇO DE ESTATISTICA DE CLIENTES" SKIP(2).

FOR EACH titulo NO-LOCK
    BREAK BY titulo.cod-emitente.

    run pi-acompanhar in h-acomp (input "Cliente: " + STRING(titulo.cod-emitente)). 

    IF FIRST-OF(titulo.cod-emitente) THEN
       ASSIGN vl-maior-tit = titulo.vl-original
              dt-maior-tit = titulo.dt-emissao
              vl-ult-tit   = titulo.vl-original
              dt-ult-tit   = titulo.dt-emissao.

    IF titulo.vl-original > vl-maior-tit THEN
       ASSIGN vl-maior-tit = titulo.vl-original 
              dt-maior-tit = titulo.dt-emissao. 

    IF titulo.dt-emissao > dt-ult-tit THEN
       ASSIGN vl-ult-tit = titulo.vl-original
              dt-ult-tit = titulo.dt-emissao.

    IF LAST-OF(titulo.cod-emitente) THEN DO:
       FIND estatist WHERE estatist.cod-emitente = titulo.cod-emitente NO-ERROR.
       IF AVAIL estatist THEN
          ASSIGN estatist.vl-maior-tit = vl-maior-tit
                 estatist.dt-maior-tit = dt-maior-tit
                 estatist.vl-ult-tit   = vl-ult-tit
                 estatist.dt-ult-tit   = dt-ult-tit.
       ELSE DO:
          ASSIGN l-erro = YES.
          PUT "Emitente: " titulo.cod-emitente " NÆo atualizado!" SKIP.
       END.
    END.
    IF l-erro = NO THEN
       DISPLAY "Todos os emitente foram atualizados!" SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
