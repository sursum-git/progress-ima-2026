/******************************************************************************
*   Programa .....: cd0708-upc.p                                              *
*   Data .........: 31/10/2002                                                *
*   Cliente ......: Ima                                                       *
*   Objetivo .....: Inclus∆o do campo usu†rio                                 *
*                                                                             *
******************************************************************************/

/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event            AS CHARACTER.
DEF INPUT PARAMETER p-ind-object           AS CHARACTER.
DEF INPUT PARAMETER p-wgh-object           AS HANDLE.
DEF INPUT PARAMETER p-wgh-frame            AS WIDGET-HANDLE.
DEF INPUT PARAMETER p-cod-table            AS CHARACTER.
DEF INPUT PARAMETER p-row-table            AS ROWID.

/****** Vari†veis ******/
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-folder          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-objeto         AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-folder                            AS CHARACTER NO-UNDO.
DEF VAR c-char                              AS CHAR.
DEF NEW GLOBAL SHARED VAR i-ct AS INTEGER.

/****** Vari†veis utilizadas para alocacao do pedido de venda (pdapi002) ******/
def var h-acomp2        AS  HANDLE   NO-UNDO .
def var h-api          AS  HANDLE   NO-UNDO .
def var de-qt-a-alocar AS  DECIMAL  NO-UNDO .
def var r-ped-ent      AS  ROWID    NO-UNDO .
def var r-saldo-estoq  AS  ROWID    NO-UNDO .
DEF NEW GLOBAL SHARED VAR v-numero-do-embarque LIKE embarque.nr-embarque.

DEF NEW GLOBAL SHARED TEMP-TABLE tt-ped-venda LIKE MOVDIS.ped-venda
    INDEX ch-ped-seq IS PRIMARY nr-pedcli.

DEF BUFFER b-embarque   FOR embarque.
DEF BUFFER b-ped-venda  FOR MOVDIS.ped-venda.
DEF BUFFER b-ped-ent    FOR MOVDIS.ped-ent.
DEF BUFFER b-ped-item   FOR MOVDIS.ped-item.
DEF BUFFER b-it-pre-fat FOR it-pre-fat.

DEF TEMP-TABLE rowerrors no-undo
    FIELD errorsequence    as int
    FIELD errornumber      as int
    FIELD errordescription as char
    FIELD errorparameters  as char
    FIELD errortype        as char
    FIELD errorhelp        as char
    FIELD errorsubtype     as char.


DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

def var c-objeto as char no-undo.
def var h_frame as widget-handle no-undo. 

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE VARIABLE var-cod-depos-aloc AS CHARACTER   NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp2.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").

IF p-ind-event  = "ROWID-EMBARQUE" AND
   p-ind-object = "CONTAINER"      THEN DO:
   FIND FIRST b-embarque WHERE ROWID(b-embarque) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL b-embarque THEN DO:
      ASSIGN v-numero-do-embarque = b-embarque.nr-embarque.
   END.
END.

DO TRANSACTION:
   IF p-ind-event  = "CREATE-TEMP-TABLE-PED-VENDA" AND
      p-ind-object = "BROWSER" THEN DO:

      FIND FIRST b-ped-venda WHERE 
           ROWID(b-ped-venda) = p-row-table NO-LOCK NO-ERROR.
      IF AVAIL b-ped-venda THEN DO:
         
         IF b-ped-venda.tp-preco = 1 AND
            b-ped-venda.cod-sit-com <> 2 THEN
            RETURN 'NOK'.
         

         IF b-ped-venda.mo-codigo <> 0 THEN DO: 
            MESSAGE "A moeda escolhida para o pedido " b-ped-venda.nr-pedcli " n∆o Ç o REAL ! ! !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN 'NOK'.
         END.
         
         FIND FIRST b-it-pre-fat WHERE
                    b-it-pre-fat.nome-abrev   = b-ped-venda.nome-abrev AND
                    b-it-pre-fat.nr-pedcli    = b-ped-venda.nr-pedcli AND
                    b-it-pre-fat.nr-embarque <> v-numero-do-embarque  
                    USE-INDEX ch-ped-item NO-LOCK NO-ERROR.

         IF AVAIL b-it-pre-fat THEN DO:
            RUN pi-inicializar IN h-acomp2 (INPUT b-it-pre-fat.nr-pedcli).
            RUN pi-finalizar IN h-acomp2. 
            RETURN 'NOK'.
         END.
      END.
   END.
END.        

PROCEDURE pi-aloca :
    DEFINE VARIABLE de-qt-alocar AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE h-alocacao AS HANDLE      NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp2.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp2 (input RETURN-VALUE).
     
    IF AVAIL ped-ent THEN DO:
       FIND FIRST saldo-estoq WHERE saldo-estoq.it-codigo   = ped-ent.it-codigo
                                and saldo-estoq.cod-refer   = ped-ent.cod-refer
                                AND saldo-estoq.cod-depos   = var-cod-depos-aloc /*tt-deposito.cod-depos*/ /*fi-deposito-aloc:SCREEN-VALUE IN FRAME {&frame-name}*/
                                and saldo-estoq.cod-localiz = ""
                                and saldo-estoq.lote        = ped-ent.cod-refer
                              NO-LOCK NO-ERROR.
          
          IF AVAIL saldo-estoq THEN DO.
             ASSIGN de-qt-alocar = ped-ent.qt-log-aloca.
             IF (saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada) < de-qt-alocar THEN 
                ASSIGN de-qt-alocar = saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada.
             
             RUN pi-acompanhar IN  h-acomp2 (INPUT "Alocando - " + STRING(saldo-estoq.it-codigo) + '-' + STRING(ped-ent.nr-pedcli) + '-' + STRING(saldo-estoq.cod-refer)).
             run pdp/pdapi002.p persistent set h-alocacao.
             run pi-aloca-fisica-man in h-alocacao(input rowid(ped-ent),
                                                   input-output de-qt-alocar, 
                                                   input rowid(saldo-estoq)).  
             DELETE PROCEDURE h-alocacao.
          END.
    END.    

    run pi-finalizar in h-acomp2.
END PROCEDURE.

PROCEDURE pi-desaloca :
    DEFINE VARIABLE de-qt-desalocar AS DECIMAL NO-UNDO.
    DEFINE VARIABLE h-alocacao      AS HANDLE  NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp2.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp2 (input RETURN-VALUE).
 
    IF AVAIL ped-ent THEN DO:
       FOR EACH ped-saldo WHERE ped-saldo.nr-pedcli = ped-ent.nr-pedcli
                            AND ped-saldo.it-codigo = ped-ent.it-codigo
                            AND ped-saldo.cod-refer = ped-ent.cod-refer
                          NO-LOCK.
           IF AVAIL ped-saldo THEN DO:
              FIND FIRST saldo-estoq WHERE saldo-estoq.cod-depos   = ped-saldo.cod-depos
                                       and saldo-estoq.cod-estabel = ped-saldo.cod-estabel
                                       and saldo-estoq.cod-localiz = ped-saldo.cod-localiz
                                       and saldo-estoq.lote        = ped-saldo.lote
                                       and saldo-estoq.it-codigo   = ped-saldo.it-codigo
                                       and saldo-estoq.cod-refer   = ped-saldo.cod-refer
                                     NO-LOCK NO-ERROR.
              
              IF AVAIL saldo-estoq THEN DO.
                 RUN  pi-acompanhar IN  h-acomp2 (INPUT "Desalocando - " + STRING(ped-saldo.it-codigo) + '-' + STRING(ped-saldo.nr-pedcli) + '-' + STRING(ped-saldo.cod-refer)).    
    
                 ASSIGN de-qt-desalocar = ped-saldo.qt-aloc-ped.
                 run pdp/pdapi002.p persistent set h-alocacao.
                 run pi-desaloca-fisica-man in h-alocacao(input rowid(ped-ent),
                                                           input-output de-qt-desalocar,
                                                           input rowid(saldo-estoq)).   
                 DELETE PROCEDURE h-alocacao.
              END.
           END.
       END.
    END.
    
    RUN pi-finalizar in h-acomp2.
END PROCEDURE.

RETURN "Ok".

