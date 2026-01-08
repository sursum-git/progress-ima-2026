/*******************************************************************************
**
**      Programa: IMBC001I.P
**      
**      DATA....: SETEMBRO DE 2002
**
**      Objetivo: ImpressÆo de Etiquetas
**
**      Versao..: 0.00.000 - Fabiano - cria‡Æo do programa
**
*******************************************************************************/
def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

def input        param r-registro       as rowid no-undo.
DEF INPUT        PARAM p-trans          AS CHAR NO-UNDO.
def input-output param                  table for tt-erro. 

DEF BUFFER bf-etiq FOR bc-etiqueta.

DEF VAR i-tp-embal      AS INTEGER.

/* {cdp/cd9000.i}  */

/*** De fini‡Æo das temp-tables para a API de consistencia do CLD tt-prog-bc***/
{bcp/bcapi004.i}

/*** Defini‡Æo das temp-tables para a API de cria‡Æo de transa‡Æo do CLD tt-trans***/
{bcp/bcapi001.i}

/*** Defini‡Æo da temp-table para a API de cria‡Æo de etiquetas tt-etiqueta***/
{bcp/bcapi002.i}

/*** Defini‡Æo da temp-table para a API de rateiro da quantidade pelo fator do item/familia ***/
/* {bcp/bcapi011.i}  */

/*** Defini‡Æo dos parƒmetros de entrada ***/

IF  SESSION:SET-WAIT-STATE("general") THEN.

find bc-etiqueta where rowid(bc-etiqueta) = r-registro
     no-lock no-error.

if  not available(bc-etiqueta) then do:
    run utp/ut-msgs.p (input "mgs",
                       input 56,
                       input "etiqueta").

    create tt-erro.
    assign tt-erro.i-sequen = 1
           tt-erro.cd-erro  = 56
           tt-erro.mensagem = return-value.

    return "NOK".
end.

/*IF  p-trans <> "IMA0004Q" THEN DO:
    find first emitente
         where emitente.nome-abrev = bc-etiqueta.nome-abrev
         no-lock no-error.
    if  not available(emitente) then do:
    
        run utp/ut-msgs.p (input "mgs",
                           input 56,
                           input "Emitente").
    
        create tt-erro.
        assign tt-erro.i-sequen = 1
               tt-erro.cd-erro  = 56
               tt-erro.mensagem = return-value.
    
        return "NOK".
    end.
END.*/

FIND FIRST ITEM WHERE
           ITEM.it-codigo = bc-etiqueta.it-codigo
           NO-LOCK NO-ERROR.
IF NOT AVAIL(ITEM) THEN DO:
   run utp/ut-msgs.p (input "mgs",
                      input 56,
                      input "Item").

   create tt-erro.
   assign tt-erro.i-sequen = 1
          tt-erro.cd-erro  = 56
          tt-erro.mensagem = return-value.

   return "NOK".
END.

/*** Cria tt-trans e tt-etiq ***/
    
create  tt-etiqueta.
assign  tt-etiqueta.cod-versao-integracao   = 1
        tt-etiqueta.i-sequen                = 1
        tt-etiqueta.quantidade              = bc-etiqueta.qt-item
        tt-etiqueta.nr-docto                = bc-etiqueta.nr-nota-fis
        tt-etiqueta.it-codigo               = bc-etiqueta.it-codigo
        tt-etiqueta.cod-estabel             = bc-etiqueta.cod-estabel
        tt-etiqueta.serie-docto             = bc-etiqueta.serie
        /*tt-etiqueta.cod-emitente            = IF  AVAIL emitente THEN emitente.cod-emitente ELSE 0*/
        tt-etiqueta.nr-sequencia            = 1
        tt-etiqueta.data-movimento          = bc-etiqueta.dt-criacao
        tt-etiqueta.cd-trans                = p-trans
        tt-etiqueta.cod-referencia          = bc-etiqueta.referencia
        tt-etiqueta.auxiliar-06             = bc-etiqueta.progressivo
        tt-etiqueta.auxiliar-07             = bc-etiqueta.progressivo.
    
RUN pi-retorna-dados (INPUT tt-etiqueta.it-codigo,
                      OUTPUT tt-etiqueta.auxiliar-01,
                      OUTPUT tt-etiqueta.auxiliar-02,
                      OUTPUT tt-etiqueta.auxiliar-03,
                      OUTPUT tt-etiqueta.auxiliar-04,
                      OUTPUT tt-etiqueta.auxiliar-05).

create tt-trans.
assign tt-trans.cod-versao-integracao = 1
       tt-trans.i-sequen              = 1
       tt-trans.cd-trans              = p-trans
       tt-trans.nr-trans              = 0
       tt-trans.usuario               = userid("mgadm")
       tt-trans.atualizada            = no
       tt-trans.etiqueta              = yes.

CASE  p-trans:
    WHEN "IMA0001Q" THEN
        ASSIGN tt-trans.detalhe = "PRG: " + bc-etiqueta.progressivo + 
                                  " ITEM:" + bc-etiqueta.it-codigo + 
                                  " QTD:" + string(bc-etiqueta.qt-item).
    WHEN "IMA0002Q" OR
    WHEN "IMA0004Q" THEN DO:
        FIND bf-etiq
            WHERE bf-etiq.nr-etiq = bc-etiqueta.nr-etiq-pai
            NO-LOCK NO-ERROR. 
        ASSIGN tt-trans.detalhe = "PRG: " + bc-etiqueta.progressivo + 
                                  " PRG PAI: " + IF AVAIL bf-etiq THEN bf-etiq.progressivo ELSE "" + 
                                  " ITEM:" + bc-etiqueta.it-codigo + 
                                  " QTD:" + string(bc-etiqueta.qt-item).
    END.
END CASE.

RAW-TRANSFER tt-etiqueta TO tt-trans.conteudo-trans.

FIND ob-etiqueta WHERE
     ob-etiqueta.progressivo = bc-etiqueta.progressivo
     NO-LOCK NO-ERROR.

IF NOT AVAIL ob-etiqueta THEN DO.
   FIND ITEM WHERE
        ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

   ASSIGN i-tp-embal = 1.
   IF ITEM.un = 'kg' THEN 
      ASSIGN i-tp-embal = 5.

   FIND corte-comerc WHERE
        corte-comerc.compr-min <= bc-etiqueta.qt-item AND
        corte-comerc.compr-max >= bc-etiqueta.qt-item AND
        corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

   CREATE ob-etiqueta.
   ASSIGN ob-etiqueta.cod-estabel     = bc-etiqueta.cod-estabel
          ob-etiqueta.dt-emissao      = TODAY
          ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
          ob-etiqueta.acondic         = ""
          ob-etiqueta.it-codigo       = bc-etiqueta.it-codigo
          ob-etiqueta.cod-refer       = bc-etiqueta.referencia
          ob-etiqueta.nr-lote         = IF bc-etiqueta.lote = '888'
                                        THEN 'RD' ELSE 'RP'
          ob-etiqueta.cod-qualid      = IF bc-etiqueta.lote = '888'
                                        THEN 'D' ELSE 'B'
          ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                        THEN corte-comerc.codigo
                                        ELSE ''
          ob-etiqueta.localizacao     = ''
          ob-etiqueta.situacao        = 3
          ob-etiqueta.quantidade      = bc-etiqueta.qt-item
          ob-etiqueta.num-etiqueta    = IF bc-etiqueta.cod-estabel = '1' 
                                        THEN NEXT-VALUE(seq-etq-estoq-ima)
                                        ELSE NEXT-VALUE(seq-etq-estoq-med)
          ob-etiqueta.progressivo     = bc-etiqueta.progressivo.
END.
ELSE DO.
   FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
   ASSIGN ob-etiqueta.quantidade = bc-etiqueta.qt-item.
   FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.
END.

RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                             INPUT ob-etiqueta.num-etiqueta,
                             INPUT NO).

/*
IF bc-etiqueta.cod-estabel = '1' THEN 
   RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                                INPUT ob-etiqueta.num-etiqueta,
                                INPUT NO).
ELSE
   RUN esapi/imp-etq-epl.p (INPUT-OUTPUT TABLE tt-etiqueta,
                            INPUT-OUTPUT TABLE tt-erro).
*/

/*
/*
IF SESSION:GET-PRINTERS() MATCHES "*TLP2844*" THEN */
   RUN esapi/imp-etq-epl.p (INPUT-OUTPUT TABLE tt-etiqueta,
                            INPUT-OUTPUT TABLE tt-erro).
/*ELSE
   RUN bcp/bcapi001.p (INPUT-OUTPUT TABLE tt-trans,
                       INPUT-OUTPUT TABLE tt-erro).*/
*/                       


/*
PAUSE 1 NO-MESSAGE.
*/
IF SESSION:SET-WAIT-STATE("") THEN.

FIND FIRST tt-erro NO-ERROR.
  
IF  AVAIL tt-erro THEN 
    RETURN "NOK".
ELSE DO:
    FIND CURRENT bc-etiqueta SHARE-LOCK.
    FIND FIRST tt-trans NO-LOCK NO-ERROR.
    IF  bc-etiqueta.cod-estado = 1 OR bc-etiqueta.cod-estado >= 7  THEN
        ASSIGN bc-etiqueta.nr-trans   = tt-trans.nr-trans
               bc-etiqueta.cd-trans   = tt-trans.cd-trans
               bc-etiqueta.cod-estado = 2.
    /*IF  p-trans = "IMA0002Q" THEN
        ASSIGN bc-etiqueta.cod-estado = 5.
    ELSE
        ASSIGN bc-etiqueta.cod-estado = 2.*/
    RETURN "OK".    
END.

PROCEDURE pi-retorna-dados:

    DEF INPUT  PARAMETER p-it-codigo  AS CHAR.
    DEF OUTPUT PARAMETER p-auxiliar-1 AS CHAR.  /* Descri‡Æo do tecido */
    DEF OUTPUT PARAMETER p-auxiliar-2 AS CHAR.  /* Largura */
    DEF OUTPUT PARAMETER p-auxiliar-3 AS CHAR.  /* Pais Origem */
    DEF OUTPUT PARAMETER p-auxiliar-4 AS CHAR.  /* Composi‡Æo */
    DEF OUTPUT PARAMETER p-auxiliar-5 AS CHAR.  /* Rendimento */
    
    FIND ITEM 
        WHERE ITEM.it-codigo = p-it-codigo 
        NO-LOCK NO-ERROR.
    
    IF  AVAIL ITEM AND 
        NUM-ENTRIES(ITEM.narrativa,CHR(10)) > 4 THEN DO:
         
         ASSIGN p-auxiliar-1 = ENTRY(3,ITEM.narrativa,CHR(10)) NO-ERROR.
         ASSIGN p-auxiliar-2 = ENTRY(5,ITEM.narrativa,CHR(10)) NO-ERROR.
         ASSIGN p-auxiliar-3 = ENTRY(4,ITEM.narrativa,CHR(10)) NO-ERROR.
         ASSIGN p-auxiliar-4 = ENTRY(6,ITEM.narrativa,CHR(10)) NO-ERROR.
         ASSIGN p-auxiliar-5 = ENTRY(7,ITEM.narrativa,CHR(10)) NO-ERROR.
    END.



END PROCEDURE.
