/******************************************************************************
**  Programa.: bodi159-upc.p
**  Objetivo.: Programa UPC do bodi159 - pd4000.w - Implantacao de Pedidos
**  Descricao: 2.04.00.000 - Desenvolvimento Inicial
*******************************************************************************/

def input param p-ind-event        as char          no-undo.
def input param p-ind-object       as char          no-undo.
def input param p-wgh-object       as handle        no-undo.
def input param p-wgh-frame        as widget-handle no-undo.
def input param p-cod-table        as char          no-undo.
def input param p-row-table        as rowid         no-undo.

def new global shared var wh-button  as widget-handle no-undo.
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var tx-label   as widget-handle no-undo.
def new global shared var h_campo    as widget-handle no-undo.

def var c-objeto as char no-undo.
def var h_frame as widget-handle no-undo. 

def new global shared var h-campo         as widget-handle no-undo.
def new global shared var h_c_cod-estabel as widget-handle no-undo.
def new global shared var h_c_nr-pedrep   as widget-handle no-undo.
def new global shared var h_c_nr-pedcli   as widget-handle no-undo.
def new global shared var h_c_nome-abrev  as widget-handle no-undo.
def new global shared var h_c_nome-rep    as widget-handle no-undo.
def new global shared var h_c_btsaveorder as widget-handle no-undo.

def new global shared var h_qt_pedida   as widget-handle no-undo.
def new global shared var h_qt_alocar   as widget-handle no-undo.
def new global shared var h_qt_disp     as widget-handle no-undo.
def new global shared var h_it_codigo   as widget-handle no-undo.
def new global shared var h_cod_refer   as widget-handle no-undo.

DEF VAR h-objeto as widget-handle no-undo.

def new global shared var gc-nome-abrev  like ped-venda.nome-abrev     no-undo.
def new global shared var gc-nr-pedcli   like ped-venda.nr-pedcli      no-undo.
def new global shared var gc-cod-estabel like ped-venda.cod-estabel    no-undo.
def new global shared var c-seg-usuario  like usuar_mestre.cod_usuario no-undo.

/*** feito dia 02/10/2002 ***/
def new global shared var wh-button  as widget-handle no-undo.
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var tx-label   as widget-handle no-undo.
def new global shared var h_campo    as widget-handle no-undo.

DEF BUFFER b-ped-venda FOR ped-venda.
DEF BUFFER b-ped-item FOR ped-item.

def new global shared var v-row-ped    AS ROWID NO-UNDO.
def new global shared var h-campo      as widget-handle no-undo.

def new global shared var h-btaddOrder                              as widget-handle no-undo.
def new global shared var h-btUpdateOrder                           as widget-handle no-undo.
def new global shared var h-btDeleteOrder                           as widget-handle no-undo.
def new global shared var h-btOrderFunctions                        as widget-handle no-undo.
def new global shared var h-btCancelationOrder                      as widget-handle no-undo.
def new global shared var h-btAllocation                            as widget-handle no-undo.
def new global shared var h-btCalculateOrder                        as widget-handle no-undo.
def new global shared var h-btCompleteOrder                         as widget-handle no-undo.
def new global shared var h-btCalculateInvoice                      as widget-handle no-undo.
def new global shared var h-btCancelOrder                           as widget-handle no-undo.
def new global shared var h-btSaveOrder                             as widget-handle no-undo.
def new global shared var h-btAddRepresentative                     as widget-handle no-undo.
def new global shared var h-btUpdateRepresentative                  as widget-handle no-undo.
def new global shared var h-btDeleteRepresentative                  as widget-handle no-undo.
def new global shared var h-btCancelRepresentative                  as widget-handle no-undo.
def new global shared var h-btSaveRepresentative                    as widget-handle no-undo.
def new global shared var h-btAddAdvPayments                        as widget-handle no-undo.
def new global shared var h-btUpdateAdvPayments                     as widget-handle no-undo.
def new global shared var h-btDeleteAdvPayments                     as widget-handle no-undo.
def new global shared var h-btCancelAdvPayments                     as widget-handle no-undo.
def new global shared var h-btSaveAdvPayments                       as widget-handle no-undo.
def new global shared var h-btAddDelivery                           as widget-handle no-undo.
def new global shared var h-btUpdateDelivery                        as widget-handle no-undo.
def new global shared var h-btDeleteDelivery                        as widget-handle no-undo.
def new global shared var h-btCancelationDelivery                   as widget-handle no-undo.
def new global shared var h-btCancelDelivery                        as widget-handle no-undo.
def new global shared var h-btSaveDelivery                          as widget-handle no-undo.
def new global shared var h-btAddTerm                               as widget-handle no-undo.
def new global shared var h-btUpdateTerm                            as widget-handle no-undo.
def new global shared var h-btDeleteTerm                            as widget-handle no-undo.
def new global shared var h-btCancelTerm                            as widget-handle no-undo.
def new global shared var h-btSaveTerm                              as widget-handle no-undo.
def new global shared var h-btDiscounts                             as widget-handle no-undo.
def new global shared var h-btUpdateBU                              as widget-handle no-undo.
def new global shared var h-btFastAddItem                           as widget-handle no-undo.
def new global shared var h-btAddItem                               as widget-handle no-undo.
def new global shared var h-btUpdateItem                            as widget-handle no-undo.
def new global shared var h-btDeleteItem                            as widget-handle no-undo.
def new global shared var h-btCancelationItem                       as widget-handle no-undo.
def new global shared var h-btCancelItem                            as widget-handle no-undo.
def new global shared var h-btSaveItem                              as widget-handle no-undo.
def new global shared var h-btgoto                                  as widget-handle no-undo.

DEF NEW GLOBAL SHARED VAR wh-cod-priori    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nome-transp   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cidade-cif    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pct-desc-inf  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR lg-add-modifica  AS LOGICAL.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").
  
    /*
                  "  Evento:"     p-ind-event                   skip
                 "  Objeto:"     p-ind-object                  skip
                 " Arquivo:"     p-wgh-object:file-name        skip
                 " Arquivo:"     p-wgh-object                  SKIP
                 "   Frame:"     p-wgh-frame:name              skip
                 "   Frame:"     p-wgh-frame              skip
                 "  Objeto: " c-objeto skip
                 "  Tabela:"     p-cod-table                   skip
                 "Registro:"     string(p-row-table) SKIP
                 "wh-fill: " wh-fill SKIP
                 "h_campo: " h_campo
                 view-as alert-box.
        */


if p-ind-event  = 'BEFORE-DISPLAY' AND
   p-ind-object = 'CONTAINER'              THEN DO:   
    assign h-objeto     = p-wgh-frame:FIRST-CHILD.
        FIND FIRST b-ped-venda WHERE ROWID(b-ped-venda) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL b-ped-venda THEN
                ASSIGN
                    gc-nome-abrev = b-ped-venda.nome-abrev
                    gc-nr-pedcli  = b-ped-venda.nr-pedcli.
END.

if p-ind-event  = 'AFTER-CONTROL-TOOL-BAR' AND
   p-ind-object = 'CONTAINER'              THEN DO:   
    assign h-objeto     = p-wgh-frame:FIRST-CHILD.
    do while valid-handle(h-objeto):       

        IF h-objeto:NAME = "nome-abrev" THEN DO:          
           ASSIGN h_c_nome-abrev = h-objeto. 
        END.       
        IF h-objeto:NAME = "nr-pedcli" THEN DO:          
          ASSIGN h_c_nr-pedcli = h-objeto.
          LEAVE.
       END.       
       if h-objeto:TYPE = 'field-group' 
          then assign h-objeto = h-objeto:FIRST-CHILD.
       else assign h-objeto = h-objeto:NEXT-SIBLING.
    end.
end.


If  p-ind-event = "BEFORE-INITIALIZE" THEN DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:    
                IF  h-objeto:NAME = "fPage1" THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" THEN DO:
                            IF h-campo:NAME = "des-pct-desconto-inform" THEN
                               ASSIGN wh-pct-desc-inf = h-campo.

                            ASSIGN h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE
                            ASSIGN h-campo = h-campo:FIRST-CHILD.
                    END.
                END.

                IF  h-objeto:NAME = "fPage3" THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):                        
                        IF h-campo:TYPE <> "field-group" THEN DO:
                            
                           IF h-campo:NAME = "no-ab-reppri" THEN 
                              ASSIGN h_c_nome-rep = h-campo. 

                           IF h-campo:NAME = "nr-pedrep" THEN
                              assign h_c_nr-pedrep = h-campo.

                           IF h-campo:NAME = "nome-transp" THEN
                              ASSIGN wh-nome-transp = h-campo.
                           
                           IF h-campo:NAME = "cidade-cif" THEN
                              ASSIGN wh-cidade-cif = h-campo.

                           assign h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE DO:
                            Assign h-campo = h-campo:first-child.
                        END.
                    END.
                END.

                IF  h-objeto:NAME = "fPage4" THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" THEN DO:
                            IF h-campo:NAME = "cod-priori" THEN DO:
                                ASSIGN wh-cod-priori = h-campo.
                                ON "LEAVE" OF h-campo PERSISTENT RUN esepc/epc-pd4000.p.
                                ON 'RETURN':U OF h-campo PERSISTENT RUN esepc/epc-pd4000r1.p.
                            END.
                            ASSIGN h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE
                            ASSIGN h-campo = h-campo:FIRST-CHILD.
                    END.
                END.
                assign h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE DO:
                Assign h-objeto = h-objeto:first-child.
            END.
        end.
    END.
END.

If p-ind-event = "AFTERINITIALIZEinterface" THEN  DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:    

                IF  h-objeto:NAME = "fPage1" THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):                        
                        IF  h-campo:TYPE <> "field-group" THEN DO:
                            
                            IF  h-campo:NAME = "COD-ESTABEL" THEN DO:
                                ASSIGN h_c_cod-estabel = h-campo
                                       gc-cod-estabel   = h_c_cod-estabel:SCREEN-VALUE. 

/*                                  h-campo:NAME skip            */
/*                                         p-ind-event SKIP             */
/*                                         h_c_cod-estabel:SCREEN-VALUE */
/*                                     VIEW-AS ALERT-BOX.               */
                                LEAVE.
                            END.
                            ELSE
                                assign h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE DO:
                            Assign h-campo = h-campo:first-child.
                        END.
                    END.
                END.
                assign h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE DO:
                Assign h-objeto = h-objeto:first-child.
            END.
        end.
    END.
END.


/*============= Descobre o Handle da qt pedida ===================*/
If p-ind-event = "AFTERINITIALIZEinterface" THEN  DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:
                
                IF  h-objeto:NAME = "fPage6" THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" THEN DO:
                            IF  h-campo:NAME = "qt-pedida" THEN DO:
                                ASSIGN h_qt_pedida  = h-campo.
                                LEAVE.
                            END.
                            ELSE
                                assign h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE DO:
                            Assign h-campo = h-campo:first-child.
                        END.
                    END.
                END.
                assign h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE DO:
                Assign h-objeto = h-objeto:first-child.
            END.
        end.
    END.
END.

/*============= Descobre o Handle da qtde dispon°vel ===================*/
If p-ind-event = "AFTERINITIALIZEinterface" THEN  DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:
                
                IF  h-objeto:NAME = "fPage6" 
                 THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" 
                        THEN DO:
                            IF  h-campo:NAME = "de-qt-dispon°vel" THEN DO:
                                ASSIGN h_qt_disp  = h-campo.
                                LEAVE.
                            END.
                            ELSE ASSIGN h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE ASSIGN h-campo = h-campo:first-child.
                    END.
                 END.
                 ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE ASSIGN h-objeto = h-objeto:first-child.
        END.
    END.
END.


/*============= Descobre o Handle do item ===================*/
If p-ind-event = "AFTERINITIALIZEinterface" THEN  DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:
                
                IF  h-objeto:NAME = "fPage6" 
                 THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" 
                        THEN DO:
                            IF  h-campo:NAME = "it-codigo" THEN DO:
                                ASSIGN h_it_codigo  = h-campo.
                                LEAVE.
                            END.
                            ELSE ASSIGN h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE ASSIGN h-campo = h-campo:first-child.
                    END.
                 END.
                 ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE ASSIGN h-objeto = h-objeto:first-child.
        END.
    END.
END.


/*============= Descobre o Handle da referencia ===================*/
If p-ind-event = "AFTERINITIALIZEinterface" THEN  DO:
    DO:
        assign h-objeto = p-wgh-frame:FIRST-CHILD.
        assign h-objeto = h-objeto:FIRST-CHILD.

        do  while valid-handle(h-objeto):
            IF  h-objeto:TYPE <> "field-group" THEN DO:
                
                IF  h-objeto:NAME = "fPage6" 
                 THEN DO:
                    ASSIGN h-campo = h-objeto:FIRST-CHILD.
                    DO  WHILE VALID-HANDLE(h-campo):
                        IF  h-campo:TYPE <> "field-group" 
                        THEN DO:
                            IF  h-campo:NAME = "cod-refer" THEN DO:

                           /*     message "h-cod-refer " h_cod_refer view-as alert-box.  */


                                ASSIGN h_cod_refer = h-campo.   
                            END.
                            IF  h-campo:NAME MATCHES "*aloc*" THEN DO:
                                ASSIGN h_qt_alocar = h-campo.
                                LEAVE.
                            END.
                            ELSE ASSIGN h-campo = h-campo:NEXT-SIBLING.
                        END.
                        ELSE ASSIGN h-campo = h-campo:first-child.
                    END.
                 END.
                 ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        END.
    END.
END.


IF VALID-HANDLE(h_qt_pedida) AND
   VALID-HANDLE(h_qt_disp)   AND
   h_qt_pedida:SENSITIVE = NO THEN DO:
   ASSIGN h_qt_disp:SCREEN-VALUE = " ".
END.


IF p-ind-event  = 'Before_pi-enableitem' AND
   VALID-HANDLE(h_qt_pedida) THEN DO:     
    FIND FIRST b-ped-venda WHERE b-ped-venda.nome-abrev = gc-nome-abrev AND
                                 b-ped-venda.nr-pedcli  = gc-nr-pedcli  NO-LOCK NO-ERROR.

    IF b-ped-venda.tp-pedido <> "SL" AND b-ped-venda.tp-pedido <> "PR" THEN
    DO:
        ON ENTRY OF h_qt_pedida PERSISTENT RUN imepc\pd4000x1.p.
    END.
END.

IF p-ind-event  = 'AfterDisplayitem' AND
   VALID-HANDLE(h_qt_pedida)  AND VALID-HANDLE(h_qt_alocar) THEN DO:     
    /* ASSIGN h_qt_alocar:SCREEN-VALUE = h_qt_pedida:SCREEN-VALUE. */
END.
            

if p-ind-event  = 'Before_pi-enableitem' AND
   p-ind-object = 'VIEWER'              THEN  DO:       
   
    FIND FIRST b-ped-venda WHERE b-ped-venda.nome-abrev = gc-nome-abrev AND
                                 b-ped-venda.nr-pedcli  = gc-nr-pedcli NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN
        IF b-ped-venda.tp-pedido <> "SL" AND b-ped-venda.tp-pedido <> "PR" THEN
        DO:
            FIND FIRST b-ped-item OF b-ped-venda NO-LOCK NO-ERROR.
            IF NOT AVAIL b-ped-item THEN
                FIND FIRST b-ped-venda 
                WHERE ROWID(b-ped-venda) = v-row-ped SHARE-LOCK NO-ERROR.
                IF AVAIL b-ped-venda 
                THEN DO:
                    FIND FIRST b-ped-item OF b-ped-venda NO-LOCK NO-ERROR.
                    IF NOT AVAIL b-ped-item THEN
                       RUN imp/impd001a.w.
                END.
        END.
           
END.

/*** feito dia 02/10/2002 ***/
IF  p-ind-event  = "AFTER-CONTROL-TOOL-BAR" AND
    p-ind-object = "CONTAINER" THEN DO:
    FIND FIRST b-ped-venda WHERE ROWID(b-ped-venda) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN
       v-row-ped = ROWID(b-ped-venda).    
END.

If  p-ind-event = "AFTER-CONTROL-TOOL-BAR" THEN DO:
    
    assign h-objeto = p-wgh-frame:FIRST-CHILD.
    assign h-objeto = h-objeto:FIRST-CHILD.

    do  while valid-handle(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:    

            IF  h-objeto:NAME BEGINS "fPage" THEN DO:
                ASSIGN h-campo = h-objeto:FIRST-CHILD.
                DO  WHILE VALID-HANDLE(h-campo):                        
                    IF  h-campo:TYPE <> "field-group" THEN DO:                                                    

                        /*** pedido ***/
                        IF  h-campo:NAME = "btAddOrder" THEN DO:                                
                            ASSIGN h-btAddOrder = h-campo.                            
                        END.
                        IF  h-campo:NAME = "btUpdateOrder" THEN DO:    
                            ASSIGN h-btUpdateOrder = h-campo.
                            ON ENTRY OF h-btUpdateOrder PERSISTENT RUN imepc/impd4000N.p.
                        END.
                        IF  h-campo:NAME = "btDeleteOrder" THEN DO:           
                            ASSIGN h-btDeleteOrder = h-campo.
                            ON ENTRY OF h-btDeleteOrder PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btOrderFunctions" THEN DO:
                            ASSIGN h-btOrderFunctions = h-campo.
                            ON ENTRY OF h-btOrderFunctions PERSISTENT RUN imepc/impd4000F.p.
                        END.
                        IF  h-campo:NAME = "btCancelationOrder" THEN DO:
                            ASSIGN h-btCancelationOrder = h-campo.
                            ON ENTRY OF h-btCancelationOrder PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btAllocation" THEN DO:
                            ASSIGN h-btAllocation = h-campo.
                            ON ENTRY OF h-btAllocation PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btCalculateOrder" THEN DO:        
                            ASSIGN h-btCalculateOrder = h-campo.
                            ON ENTRY OF h-btCalculateOrder PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btCompleteOrder" THEN DO:         
                            ASSIGN h-btCompleteOrder = h-campo.
                            ON 'MOUSE-SELECT-CLICK' OF h-btCompleteOrder PERSISTENT RUN imepc/impd4000M.p.
                        END.                        
                        IF  h-campo:NAME = "btCalculateInvoice" THEN DO:      
                            ASSIGN h-btCalculateInvoice = h-campo.
                            ON ENTRY OF h-btCalculateInvoice PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        /*** repres ***/
                        IF  h-campo:NAME = "btAddRepresentative" THEN DO:     
                            ASSIGN h-btAddRepresentative = h-campo.                            
                            ON ENTRY OF h-btAddRepresentative PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btUpdateRepresentative" THEN DO:  
                            ASSIGN h-btUpdateRepresentative = h-campo.
                            ON ENTRY OF h-btUpdateRepresentative PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btDeleteRepresentative" THEN DO:  
                            ASSIGN h-btDeleteRepresentative = h-campo.
                            ON ENTRY OF h-btDeleteRepresentative PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        /*** cond-pagto ***/
                        IF  h-campo:NAME = "btAddAdvPayments" THEN DO:        
                            ASSIGN h-btAddAdvPayments = h-campo.                            
                            ON ENTRY OF h-btAddAdvPayments PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btUpdateAdvPayments" THEN DO:     
                            ASSIGN h-btUpdateAdvPayments = h-campo.
                            ON ENTRY OF h-btUpdateAdvPayments PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btDeleteAdvPayments" THEN DO:     
                            ASSIGN h-btDeleteAdvPayments = h-campo.
                            ON ENTRY OF h-btDeleteAdvPayments PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        /*** pe ***/
                        IF  h-campo:NAME = "btAddDelivery" THEN DO:           
                            ASSIGN h-btAddDelivery = h-campo.                            
                            ON ENTRY OF h-btAddDelivery PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btUpdateDelivery" THEN DO:        
                            ASSIGN h-btUpdateDelivery = h-campo.
                            ON ENTRY OF h-btUpdateDelivery PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btDeleteDelivery" THEN DO:        
                            ASSIGN h-btDeleteDelivery = h-campo.
                            ON ENTRY OF h-btDeleteDelivery PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btCancelationDelivery" THEN DO:   
                            ASSIGN h-btCancelationDelivery = h-campo.
                            ON ENTRY OF h-btCancelationDelivery PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        /*** pedido ***/
                        IF  h-campo:NAME = "btAddTerm" THEN DO:               
                            ASSIGN h-btAddTerm = h-campo.                            
                            ON ENTRY OF h-btAddTerm PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btUpdateTerm" THEN DO:            
                            ASSIGN h-btUpdateTerm = h-campo.
                            ON ENTRY OF h-btUpdateTerm PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btDeleteTerm" THEN DO:            
                            ASSIGN h-btDeleteTerm = h-campo.
                            ON ENTRY OF h-btDeleteTerm PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        /***
                        /*** pedido ***/
                        IF  h-campo:NAME = "btDiscounts" THEN DO:             
                            ASSIGN h-btDiscounts = h-campo.
                            ON ENTRY OF h-btDiscounts PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        /*** pedido ***/
                        IF  h-campo:NAME = "btUpdateBU" THEN DO:              
                            ASSIGN h-btUpdateBU = h-campo.
                            ON ENTRY OF h-btUpdateBU PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        ***/

                        /*** item pedido ***/
                        IF  h-campo:NAME = "btFastAddItem" THEN DO:           
                            ASSIGN h-btFastAddItem = h-campo.                            
                            ON ENTRY OF h-btFastAddItem PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btAddItem" THEN DO:
                            ASSIGN h-btAddItem = h-campo.                            
                            ON ENTRY OF h-btAddItem PERSISTENT RUN imepc/impd4000K.p.
                        END.
                        IF  h-campo:NAME = "btUpdateItem" THEN DO:            
                            ASSIGN h-btUpdateItem = h-campo.
                            ON ENTRY OF h-btUpdateItem PERSISTENT RUN imepc/impd4000L.p.
                        END.
                        IF  h-campo:NAME = "btDeleteItem" THEN DO:            
                            ASSIGN h-btDeleteItem = h-campo.
                            ON ENTRY OF h-btDeleteItem PERSISTENT RUN imepc/pd4000-entry.p.
                        END.
                        IF  h-campo:NAME = "btCancelationItem" THEN DO:       
                            ASSIGN h-btCancelationItem = h-campo.
                            ON ENTRY OF h-btCancelationItem PERSISTENT RUN imepc/pd4000-entry.p.
                        END.

                        assign h-campo = h-campo:NEXT-SIBLING.
                    END.
                    ELSE Assign h-campo = h-campo:first-child.
                END.
            END.
            assign h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE Assign h-objeto = h-objeto:first-child.          
    END.
END.


if p-ind-object = 'CONTAINER' AND
   VALID-HANDLE(p-wgh-frame) THEN DO.

    assign h-objeto     = p-wgh-frame:FIRST-CHILD.
    do while valid-handle(h-objeto):       
        
       IF h-objeto:NAME = "btgoto" THEN DO:          
          ASSIGN h-btgoto = h-objeto.
          LEAVE.
       END.
              
       if h-objeto:TYPE = 'field-group' 
          then assign h-objeto = h-objeto:FIRST-CHILD.
       else assign h-objeto = h-objeto:NEXT-SIBLING.
    end.
end.


/***************************************************************************************
*   Programa .....: IMPD4000.P
*   Data .........: Dezembro/2004     
*   Cliente ......: IMA               
*   Programador ..: Aroldo                                                  
*   Objetivo .....: Acrescentar zoom no campo tipo de pedido.
*                   Validar o campo tipo de pedido.
*                   Criar amarraá∆o entre PV X PC.
****   Alterado ****
*   Data .........: Janeiro/2004
*   Programador...: Bruno Hallais
*   Objetivo......: 1- AlÇm de acrescentar zoom ao campo tp-pedido, valida o mesmo.
                    2- Ainda verifica no momento que clica no bot∆o de criar itens,
                    se o tp-pedido est† preenchido e caso negativo, cancela inclus∆o.
                    3- Ao incluir itens, receber a qtdade pedida e, ao confirmar a
                    inclus∆o, testar se h† saldo ou n∆o. Cancelar inclus∆o caso negativo.
*****************************************************************************************/

DEF NEW GLOBAL SHARED VAR tp-pedido-pd4000  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btCancelItem   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qtd-pedida     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-it-codigo      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-refer      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nr-pedcli      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nome-abrev     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-dt-implant     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-SaveItem    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-sequencia      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-primeiro-campo AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-it-codigo-pd4000  AS CHAR FORMAT "X(16)".
DEF NEW GLOBAL SHARED VAR c-cod-refer-pd4000  AS CHAR FORMAT "X(8)".
DEF NEW GLOBAL SHARED VAR c-nr-pedcli-pd4000  AS CHAR FORMAT "X(12)".
DEF NEW GLOBAL SHARED VAR c-nome-abrev-pd4000 AS CHAR FORMAT "X(14)".
DEF NEW GLOBAL SHARED VAR i-cod-emit-pd4000   LIKE ped-venda.cod-emitente.
DEF NEW GLOBAL SHARED VAR dt-implant-pd4000   AS DATE FORMAT "99/99/9999".
DEF NEW GLOBAL SHARED VAR lg-altera           AS LOGICAL.

DEF VAR wh-objeto    AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame      AS WIDGET-HANDLE NO-UNDO.

DEF VAR l-tp-pedido-preenchido AS LOG INIT YES.

/* message
      "--------------------------------------------------------------" SKIP
       "p-ind-event = "   p-ind-event SKIP
       "p-ind-object = "  p-ind-object SKIP
  /*     "p-wgh-object = "   p-wgh-objec SKIP   */
       "p-wgh-frame = "   p-wgh-frame   SKIP   
       "p-cod-table = "   p-cod-table SKIP  
    /*   "p-row-table = "  p-row-table SKIP   */  
       "c-objeto = "   c-objeto SKIP    
       "--------------------------------------------------------------" SKIP view-as alert-box.  */


/*
RUN pi-gera-log.
*/

IF p-ind-event = "AFTER-INITIALIZE" AND p-ind-object = "CONTAINER" THEN DO:
   ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
          h-frame = h-frame:FIRST-CHILD.
   DO WHILE h-frame <> ?: 
      IF h-frame:TYPE <> "field-group" THEN DO:
         ASSIGN h-campo = h-frame.
         
         IF TRIM(h-campo:NAME) = "btOrderparameters" THEN DO:
            ON "ENTRY" OF h-campo PERSISTENT RUN imepc/impd4000P.p. /*Verifica se pode incluir itens. Testa se o pedido j† tem Tipo definido.*/
         END.

         IF TRIM(h-campo:NAME) = 'nr-pedcli' THEN DO:
            ASSIGN wh-nr-pedcli = h-campo. 
         END.

         IF TRIM(h-campo:NAME) = 'nome-abrev' THEN DO:
            ON "ENTRY" OF h-campo PERSISTENT RUN imepc/impd4000Q.p. /*Verifica se pode incluir itens. Testa se o pedido j† tem Tipo definido.*/
            ASSIGN wh-nome-abrev = h-campo. 
         END.

         IF TRIM(h-campo:NAME) = "fpage4" THEN DO:
            RUN pi-zoom-tp-pedido.
         END.

         IF TRIM(h-campo:NAME) = "fpage1" THEN DO:
             RUN pi-pedido.
         END.

         IF TRIM(h-campo:NAME) = "fpage6" THEN DO:
             RUN pi-cria-primeiro-campo.
         END.
         ASSIGN h-frame = h-frame:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-frame = h-frame:FIRST-CHILD.    
   END.
END.

IF  p-ind-event  = "Before_pi-enableitem" AND p-ind-object = "viewer" THEN DO:

    ASSIGN dt-implant-pd4000   = ?
           c-nr-pedcli-pd4000  = "" 
           c-nome-abrev-pd4000 = ""
           i-cod-emit-pd4000   = 0
           c-it-codigo-pd4000  = ""
           c-cod-refer-pd4000  = "".

   ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
          h-frame = h-frame:FIRST-CHILD.
   DO WHILE h-frame <> ?: 
      IF h-frame:TYPE <> "field-group" THEN DO:
         ASSIGN h-campo = h-frame.

         IF TRIM(h-campo:NAME) = 'nr-pedcli' THEN DO:
            ASSIGN wh-nr-pedcli = h-campo. 
         END.

         IF TRIM(h-campo:NAME) = 'nome-abrev' THEN DO:
            ASSIGN wh-nome-abrev = h-campo. 
         END.

         IF TRIM(h-campo:NAME) = "fpage1" THEN DO:
            RUN pi-busca-dt-implant.
         END.

         IF TRIM(h-campo:NAME) = "fpage6" THEN DO:
            RUN pi-valida-itens.
         END.

         ASSIGN h-frame = h-frame:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-frame = h-frame:FIRST-CHILD.    
   END.
END.

PROCEDURE pi-zoom-tp-pedido.
   ASSIGN wh-objeto  = h-campo:FIRST-CHILD.
   
   DO WHILE VALID-HANDLE(wh-objeto):
      
      PUT UNFORMATTED "           " wh-objeto:NAME SKIP.
      IF TRIM(wh-objeto:NAME) = 'tp-pedido' THEN DO:
         ASSIGN tp-pedido-pd4000 = wh-objeto.
         IF tp-pedido-pd4000:LOAD-MOUSE-POINTER("image\lupa.cur") THEN .
         ON "F5", "MOUSE-SELECT-DBLCLICK" OF tp-pedido-pd4000 PERSISTENT RUN imepc/impd4000A.p . /*Zoom do campo tp-pedido*/
         ON "LEAVE" OF tp-pedido-pd4000 PERSISTENT RUN imepc/impd4000B.p.  /*Leave do campo tp-pedido*/
         ON "ENTRY" OF tp-pedido-pd4000 PERSISTENT RUN imepc/impd4000o.p.  /*ENTRY di canoi tp-pedido*/
      END.
      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
   END.
END.

PROCEDURE pi-pedido.
   ASSIGN h-campo = h-frame.
   ASSIGN wh-objeto  = h-campo:FIRST-CHILD.

   ASSIGN lg-add-modifica = NO.


   DO WHILE VALID-HANDLE(wh-objeto):

      IF wh-objeto:NAME = "btAddOrder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000R.p.
      END.

      IF wh-objeto:NAME = 'btFastAddOrder' THEN DO.
         ON 'mouse-select-click':U OF wh-objeto PERSISTENT RUN imepc/epc-pd4000m1.p (INPUT p-wgh-frame). 
      END.

      IF wh-objeto:NAME = "btCopyOrder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000S.p.
      END.

      IF wh-objeto:NAME = "btCancelorder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000T.p.
      END.

      IF wh-objeto:NAME = "btSaveorder" THEN DO:
          ON "ENTRY" OF wh-objeto PERSISTENT RUN esepc/epc-pd4000e2.p (INPUT l-tp-pedido-preenchido). 
      END.

      IF wh-objeto:NAME = "btUpdateorder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000N.p. /*Verifica se pode incluir itens. Testa se o pedido j† tem Tipo definido.*/
      END.
      
      /*
      IF wh-objeto:NAME = "btDeleteOrder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000C.p. /*Desfazer a vinculaá∆o de controle de saldo de item para pedidos deletados.*/
      END.
      */
      
      IF wh-objeto:NAME = "btOrderFunctions" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000F.p. /*Desfazer a vinculaá∆o de controle de saldo de item para pedidos suspensos*/
      END.

      /*
      IF wh-objeto:NAME = "btCancelationOrder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000H.p. /*Desfazer a vinculaá∆o de controle de saldo de item para pedidos cancelados*/
      END.
      */
      /*
      ton
      IF wh-objeto:NAME = "btCompleteOrder" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000M.p. /*Desfazer a vinculaá∆o de controle de saldo de item para pedidos cancelados*/
      END.
      */

      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
   END.
END.

PROCEDURE pi-cria-primeiro-campo.
   ASSIGN wh-objeto  = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(wh-objeto):

      IF TRIM(wh-objeto:NAME) = 'nr-sequencia' THEN DO: 
         ASSIGN wh-sequencia = wh-objeto.
         IF NOT VALID-HANDLE(wh-primeiro-campo) THEN DO:
            create FILL-IN wh-primeiro-campo  /*Criando primeiro-campo da tela de itens*/
              assign NAME              = "wh-primeiro-campo"
                     frame             = wh-objeto:FRAME
                     DATA-TYPE         = "CHARACTER"
                     FORMAT            = "X(1)"
                     WIDTH             = 0.01
                     HEIGHT            = 0.01
                     ROW               = wh-objeto:ROW
                     COL               = wh-objeto:COL
                     FGCOLOR           = 0
                     VISIBLE           = TRUE
                     SENSITIVE         = TRUE
                 TRIGGERS:
                    ON ENTRY PERSISTENT RUN imepc/impd4000G.p. /*Pegar o pedido*/
                 END TRIGGERS.
         END.
      END.
      
      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.

    END.
END.

PROCEDURE pi-valida-itens.
   ASSIGN wh-objeto  = h-campo:FIRST-CHILD.

   DO WHILE VALID-HANDLE(wh-objeto):
       
      IF TRIM(wh-objeto:NAME) = 'nr-sequencia' THEN DO: 
         ASSIGN wh-sequencia = wh-objeto.

         IF NOT VALID-HANDLE(wh-primeiro-campo) THEN DO:
            create FILL-IN wh-primeiro-campo  /*Criando primeiro-campo da tela de itens*/
            assign NAME              = "wh-primeiro-campo"
                   frame             = wh-objeto:FRAME
                   DATA-TYPE         = "CHARACTER"
                   FORMAT            = "X(1)"
                   WIDTH             = 0.01
                   HEIGHT            = 0.01
                   ROW               = wh-objeto:ROW
                   COL               = wh-objeto:COL
                   FGCOLOR           = 0
                   VISIBLE           = TRUE
                   SENSITIVE         = TRUE
                 TRIGGERS:
                    ON ENTRY PERSISTENT RUN imepc/impd4000G.p. /*Pegar o pedido*/
                 END TRIGGERS.
         END.

         IF VALID-HANDLE(wh-primeiro-campo) THEN DO:
            APPLY "entry" TO wh-primeiro-campo.
         END.

         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      
      IF wh-objeto:NAME = "it-codigo" THEN DO:
          ASSIGN wh-it-codigo = wh-objeto.
          ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF wh-objeto:NAME = "cod-refer" THEN DO:
          ASSIGN wh-cod-refer = wh-objeto.
      END. 

      IF trim(wh-objeto:NAME) = "cod-refer" THEN DO:

/*      message "passando 1111 " trim(wh-objeto:NAME)  view-as alert-box.   */

          ASSIGN wh-cod-refer = wh-objeto.
          ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/pd4000x1-vi.p.  
          ON "LEAVE" OF wh-objeto PERSISTENT RUN imepc/impd4000DR.p.  /* by rodrigo 28/04/2006 */
          ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000DR.p.
      END.

/*
          ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.   */
      
      

      IF TRIM(wh-objeto:NAME) = 'qt-pedida' THEN DO:
         ASSIGN wh-qtd-pedida = wh-objeto.
         ON "ENTRY"   OF wh-objeto PERSISTENT RUN imepc/pd4000x1-vi.p.
         ON "LEAVE"   OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      
      /*inserido por Viviane em 03/03/2006 para fazer a verificacao da qt-pedida tambem quando for teclado o enter*/
      IF TRIM(wh-objeto:NAME) = 'qt-log-aloca' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'nr-tabpre' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.
      
      IF TRIM(wh-objeto:NAME) = 'vl-preori' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'log-usa-tabela-desconto' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'des-pct-desconto-inform' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'val-pct-desconto-tab-preco' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'val-desconto-inform' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'c-conta-contabil' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'nr-ordem' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'parcela' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'de-custo-contabil' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.

      IF TRIM(wh-objeto:NAME) = 'c-class-fiscal' THEN DO:
         ON "RETURN" OF wh-objeto PERSISTENT RUN imepc/impd4000D.p.
      END.
      /**********/
      

      IF wh-objeto:NAME = "btCancelItem" THEN DO:
         ASSIGN wh-btcancelitem = wh-objeto.
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000T.p.
      END.

      IF wh-objeto:NAME = "btUpdateItem" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000L.p.
      END.

      IF wh-objeto:NAME = "btAddItem" THEN DO:
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000K.p.
      END.

      /*
      IF wh-objeto:NAME = "btSaveItem" THEN DO:
         ASSIGN wh-bt-SaveItem = wh-objeto.
         ON "ENTRY" OF wh-objeto PERSISTENT RUN imepc/impd4000E.p. /*Verifica se pode incluir itens. Testa se o pedido j† tem Tipo definido.*/
      END.
      */
      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.

    END.
END.

PROCEDURE pi-busca-dt-implant.
   ASSIGN wh-objeto  = h-campo:FIRST-CHILD. 
   DO WHILE VALID-HANDLE(wh-objeto):
       
      IF TRIM(wh-objeto:NAME) = 'dt-implant' THEN DO: /*Verifica se o tp-pedido est† preenchido ou n∆o. Ele Ç obrigat¢rio.*/
         ASSIGN wh-dt-implant = wh-objeto
                dt-implant-pd4000 = DATE(wh-objeto:SCREEN-VALUE).
      END.

      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
   END.
END.

PROCEDURE pi-gera-log:
   IF p-ind-event = "BEFORE-INITIALIZE" AND p-ind-object = "CONTAINER" THEN
      OUTPUT TO c:\lixo.txt.
   ELSE
      OUTPUT TO c:\lixo.txt APPEND.
   PUT "--------------------------------------------------------------" SKIP
       "p-ind-event = "  '"' + TRIM(p-ind-event)  + '"'  FORMAT "x(30)" SKIP
       "p-ind-object = " '"' + TRIM(p-ind-object) + '"'  FORMAT "x(30)" SKIP
       "p-wgh-object = " p-wgh-object                                   SKIP
       "p-wgh-frame = "  p-wgh-frame                                    SKIP
       "p-cod-table = "  '"' + TRIM(p-cod-table) + '"'   FORMAT "x(30)" SKIP
       "p-row-table = "  STRING(p-row-table)             FORMAT "x(30)" SKIP
       "c-objeto = "     '"' + TRIM(c-objeto)    + '"'   FORMAT "x(50)" SKIP
       "--------------------------------------------------------------" SKIP.
   OUTPUT CLOSE.
END PROCEDURE. /* pi-gera-log */


RETURN "Ok".
