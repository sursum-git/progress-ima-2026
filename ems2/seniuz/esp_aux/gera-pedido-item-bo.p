/********************************************************************************

    Programa    : gera-pedido
    
    Objetivo    : Gera Pedido no EMS 2.06 a apartir da proposta/of
    
    Criado em   : 30/05/205
    
    Autor       :  Eline Maria Ferreira Julio
    
    Altera‡Æo   : 
    
********************************************************************************/
/*************************************************
* i_dbeai.i - Include de intala»’o do EAI 
**************************************************/

DEFINE NEW GLOBAL SHARED VARIABLE v_log_eai_habilit AS LOGICAL NO-UNDO INITIAL NO.

   DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren AS CHAR FORMAT "X(12)" NO-UNDO.


   def temp-table tt-ped-item like ped-item 
       field r-rowid as rowid.

   def temp-table tt-ped-item2 like ped-item 
       field r-rowid as rowid.

   DEF INPUT PARAMETER TABLE FOR tt-ped-item.
   DEF INPUT PARAM row-ped-venda AS ROWID.

   def var h-bodi159    as handle no-undo.
   def var h-bodi159sdf as handle no-undo.
   def var h-bodi154    as handle no-undo.
   def var h-bodi157    as handle no-undo.
   def var h-bodi154sdf as handle no-undo.


   DEF TEMP-TABLE rowErrors NO-UNDO    
       FIELD errorsequence    AS INT
       FIELD errornumber      AS INT
       FIELD errordescription AS CHAR FORMAT "x(150)"
       FIELD errorparameters  AS CHAR
       FIELD errortype        AS CHAR
       FIELD errorhelp        AS CHAR FORMAT "x(150)"
       FIELD errorsubtype     AS CHAR.

   run dibo/bodi159.p  persistent set h-bodi159.
   run openQueryStatic in h-bodi159 ("main").

   run dibo/bodi154.p  persistent set h-bodi154.
   run openQueryStatic in h-bodi154 ("main").

   run dibo/bodi157.p  persistent set h-bodi157.
   run openQueryStatic in h-bodi157 ("main").

   run dibo/bodi159sdf.p  persistent set h-bodi159sdf.

   run dibo/bodi154sdf.p  persistent set h-bodi154sdf.


FIND FIRST tt-ped-item NO-LOCK NO-ERROR.
CREATE tt-ped-item2.
BUFFER-COPY tt-ped-item TO tt-ped-item2.

run getRowid                    in h-bodi159 (output row-ped-venda).


run inputTable     in h-bodi154sdf (input table tt-ped-item).
RUN setDefaultItem in h-bodi154sdf.

run outputTable    in h-bodi154sdf (output table tt-ped-item). 

FIND FIRST tt-ped-item NO-ERROR.
FIND FIRST tt-ped-item2 NO-ERROR.


FIND item-dist WHERE item-dist.it-codigo = tt-ped-item2.it-codigo NO-LOCK NO-ERROR.
IF NOT AVAIL item-dist THEN DO:
    CREATE item-dist.
    ASSIGN item-dist.it-codigo = tt-ped-item2.it-codigo.
END.


ASSIGN tt-ped-item.dt-entori           = tt-ped-item2.dt-entori            
       tt-ped-item.dt-entrega          = tt-ped-item2.dt-entrega           
       tt-ped-item.cod-entrega         = tt-ped-item2.cod-entrega          
       tt-ped-item.dt-entrega          = tt-ped-item2.dt-entrega           
       tt-ped-item.cod-entrega         = tt-ped-item2.cod-entrega          
       tt-ped-item.nat-oper            = tt-ped-item2.nat-oper             
       tt-ped-item.vl-preori           = tt-ped-item2.vl-preori            
       tt-ped-item.vl-pretab           = tt-ped-item2.vl-pretab            
       tt-ped-item.vl-preuni           = tt-ped-item2.vl-preuni            
       tt-ped-item.vl-preori-un-fat    = tt-ped-item2.vl-preori-un-fat     
       tt-ped-item.aliquota-ipi        = tt-ped-item2.aliquota-ipi                      
       tt-ped-item.dec-2               = tt-ped-item2.dec-2                 
       tt-ped-item.char-2              = tt-ped-item2.char-2               
       tt-ped-item.tipo-atend          = tt-ped-item2.tipo-atend                                            
       tt-ped-item.cod-sit-item        = tt-ped-item2.cod-sit-item .    


/*MESSAGE tt-ped-item.vl-preuni tt-ped-item.qt-pedida VIEW-AS ALERT-BOX.*/


run setRecord    in h-bodi154 (input table tt-ped-item).
run createRecord in h-bodi154.



run getRowErrors in h-bodi154 (output table Rowerrors).


        if  can-find(first RowErrors
                     where RowErrors.ErrorType <> "INTERNAL":U) then do:
            FOR EACH  RowErrors  where RowErrors.ErrorType <> "INTERNAL" NO-LOCK:
                RUN esp\util\mensagem (INPUT "NÆo Gerou Item do Pedido! (Ped-item)",
                                       INPUT "NÆo foi possivel gerar o item do pedido." + errorDescription + " " + errorhelp + " " + STRING(errornumber) ).

            END.
        end.

