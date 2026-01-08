/********************************************************************************

    Programa    : gera-pedido
    
    Objetivo    : Gera Pedido no EMS 2.06 a apartir da proposta/of
    
    Criado em   : 30/05/205
    
    Autor       :  Eline Maria Ferreira Julio
    
    Alteraá∆o   : 
    
********************************************************************************/
/*************************************************
* i_dbeai.i - Include de intalaªío do EAI 
**************************************************/

DEFINE NEW GLOBAL SHARED VARIABLE v_log_eai_habilit AS LOGICAL NO-UNDO INITIAL NO.

&GLOBAL-DEFINE EAI_inst YES


   DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren AS CHAR FORMAT "X(12)" NO-UNDO.

   def temp-table tt-ped-venda like ped-venda 
       field r-rowid as rowid.

   def temp-table tt-ped-item like ped-item 
       field r-rowid as rowid.

   def temp-table tt-ped-repre like ped-repre 
       FIELD nr-pedcli AS CHAR 
       field r-rowid as rowid.

   def temp-table tt-ped-ent like ped-ent 
       field r-rowid as rowid.

   def temp-table tt-cond-ped like cond-ped 
       FIELD nr-pedcli LIKE ped-venda.nr-pedcli
       field r-rowid as rowid.

   def temp-table tt-upc-ped-venda like upc-ped-venda 
       field r-rowid as rowid.

   def temp-table tt-upc-cond-ped like upc-cond-ped 
       field r-rowid as rowid.

   def temp-table tt-ped-venda2 like ped-venda 
       field r-rowid as rowid.

   def temp-table tt-ped-item2 like ped-item 
       field r-rowid as rowid.

   def temp-table tt-cond-ped2 like cond-ped 
       field r-rowid as rowid.


    DEF INPUT PARAMETER TABLE FOR tt-ped-venda.
    DEF INPUT PARAMETER TABLE FOR tt-ped-item.
    DEF INPUT PARAMETER TABLE FOR tt-ped-repre.
    DEF INPUT PARAMETER TABLE FOR tt-ped-ent.
    DEF INPUT PARAMETER TABLE FOR tt-cond-ped.
    DEF INPUT PARAMETER TABLE FOR tt-upc-ped-venda.
    DEF INPUT PARAMETER TABLE FOR tt-upc-cond-ped.
    DEF INPUT PARAM c-nr-pedcli AS CHAR NO-UNDO.

    def var h-bodi159    as handle no-undo.
    def var h-bodi159sdf as handle no-undo.
    def var h-bodi154    as handle no-undo.
    def var h-bodi157    as handle no-undo.
    def var h-bodi154sdf as handle no-undo.

    FIND FIRST tt-ped-venda WHERE tt-ped-venda.nr-pedcli = c-nr-pedcli NO-ERROR.
    IF AVAIL tt-ped-venda THEN DO:
        FIND FIRST tt-ped-item OF tt-ped-venda NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-item THEN tt-ped-venda.nat-operacao = tt-ped-item.nat-operacao.
    END.


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



    FOR EACH tt-ped-venda WHERE  tt-ped-venda.nr-pedcli = c-nr-pedcli.
        BUFFER-COPY tt-ped-venda TO tt-ped-venda2.
    END.


    find first tt-ped-venda2 WHERE tt-ped-venda2.nr-pedcli = c-nr-pedcli  no-error.
    if  avail tt-ped-venda2 then do:
     /*   FOR EACH tt-cond-ped WHERE  tt-cond-ped.nr-pedido = tt-ped-venda2.nr-pedido.
            BUFFER-COPY tt-cond-ped TO tt-cond-ped2.
        END.*/



        run setUserLog         IN h-bodi159sdf (input  v_cod_usuar_corren).
        run inputTable         in h-bodi159sdf (input table tt-ped-venda2).
        RUN setDefaultCustomer IN h-bodi159sdf.

/*        MESSAGE "gera-pedido-bo"  tt-ped-venda2.nr-tab-finan tt-ped-venda2.nr-ind-fin tt-ped-venda2.tab-ind-fin VIEW-AS ALERT-BOX.*/


/*        RUN outputTable        IN h-bodi159sdf (OUTPUT TABLE tt-ped-venda2).
       /*   os campos dever∆o ser gravados aqui, mas os campos chaves devem ser informados antes */

        FIND FIRST tt-ped-venda22 NO-LOCK NO-ERROR.

        find first tt-ped-venda2 WHERE tt-ped-venda2.nr-pedcli = c-nr-pedcli no-lock no-error.
        ASSIGN tt-ped-venda2.NAT-OPER = tt-ped-venda22.NAT-OPER. */


/*        MESSAGE "entrega" tt-ped-venda2.dt-entrega "emiss" tt-ped-venda2.dt-emissao "impl" tt-ped-venda2.dt-impl 
            tt-ped-item2.dt-entrega  "Pedido" tt-ped-venda2.nr-pedido VIEW-AS ALERT-BOX.
*/

        run emptyRowErrors in h-bodi159.
        
        run setRecord      in h-bodi159 (input table tt-ped-venda2).
        
        run createRecord   in h-bodi159.
        

        run getRowErrors   in h-bodi159 (output table Rowerrors).


        FIND FIRST Rowerrors NO-LOCK NO-ERROR.
        IF NOT AVAIL Rowerrors THEN  DO:
            RUN outputTable        IN h-bodi159sdf (OUTPUT TABLE tt-ped-venda2).

            FIND FIRST tt-ped-venda2 WHERE tt-ped-venda2.nr-pedcli = c-nr-pedcli NO-ERROR.
            

/*            run createOrdersRepresentatives in h-bodi157 (input tt-ped-venda2.r-rowid). */

             FOR EACH tt-ped-item WHERE  tt-ped-item.nr-pedcli = c-nr-pedcli.
                 FOR EACH  tt-ped-item2.
                     DELETE tt-ped-item2.
                 END.
                 CREATE tt-ped-item2.
                 BUFFER-COPY tt-ped-item TO tt-ped-item2.
                 RUN esp/scp/scpinc/gera-pedido-item-bo.p (INPUT TABLE tt-ped-item2,
                                            INPUT tt-ped-venda2.r-rowid).
             END.


        END.
        
/*        MESSAGE "2" tt-ped-venda2.nr-tab-finan tt-ped-venda2.nr-ind-fin VIEW-AS ALERT-BOX.*/

       for each rowErrors :
           RUN esp\util\mensagem (INPUT "N∆o Gerou Pedido! (ped-venda)",
                                  INPUT "N∆o foi possivel gerar o pedido da OF:" + c-nr-pedcli + "!" + errorDescription).

/*            message errorNumber       skip
                    errorDescription  view-as alert-box. */
        end.
        FIND FIRST Rowerrors NO-LOCK NO-ERROR.
        IF NOT AVAIL Rowerrors THEN  RUN pi-cria-outros.

    end.

    delete procedure h-bodi159.
    delete procedure h-bodi154.
    delete procedure h-bodi157.
    delete procedure h-bodi159sdf.
    delete procedure h-bodi154sdf.




    PROCEDURE pi-cria-outros.


/*        MESSAGE "gera-pedido" c-nr-pedcli VIEW-AS ALERT-BOX.*/

        FIND FIRST tt-ped-venda2 WHERE tt-ped-venda2.nr-pedcli = c-nr-pedcli NO-ERROR.
        FIND FIRST ped-venda WHERE ped-venda.nr-pedcli = tt-ped-venda2.nr-pedcli AND
                                   ped-venda.nome-abrev = tt-ped-venda2.nome-abrev NO-LOCK NO-ERROR.   

        FOR EACH tt-ped-repre WHERE tt-ped-repre.nr-pedcli = ped-venda.nr-pedcli  NO-LOCK:

            CREATE ped-repre.
            ASSIGN ped-repre.cod-classificador                     = tt-ped-repre.cod-classificador            
                   ped-repre.comis-emis                            = tt-ped-repre.comis-emis                   
                   ped-repre.idi-liber-pagto-comis-agent           = tt-ped-repre.idi-liber-pagto-comis-agent  
                   ped-repre.idi-tip-base-comis-agent              = tt-ped-repre.idi-tip-base-comis-agent                       
                   ped-repre.idi-tip-comis-agent                   = tt-ped-repre.idi-tip-comis-agent                            
                   ped-repre.ind-repbase                           = NO                                    
                   ped-repre.nome-ab-rep                           = tt-ped-repre.nome-ab-rep                  
                   ped-repre.nr-pedido                             = ped-venda.nr-pedido                 
                   ped-repre.perc-comis                            = tt-ped-repre.perc-comis.                   
          
        END.
   /*     FOR EACH tt-ped-ent WHERE tt-ped-ent.nr-pedcli = ped-venda.nr-pedcli AND
                                  tt-ped-ent.nome-abrev = ped-venda.nome-abrev NO-LOCK:

            CREATE ped-ent.
            ASSIGN ped-ent.nr-pedcli      = tt-ped-ent.nr-pedcli  
                   ped-ent.nome-abrev     = tt-ped-ent.nome-abrev  
                   ped-ent.nr-sequencia   = tt-ped-ent.nr-sequencia  
                   ped-ent.it-codigo      = tt-ped-ent.it-codigo  
                   ped-ent.qt-pedida      = tt-ped-ent.qt-pedida  
                   ped-ent.qt-un-fat      = tt-ped-ent.qt-un-fat  
                   ped-ent.dt-entrega     = tt-ped-ent.dt-entrega .
        END.*/


        /*
        FOR EACH tt-cond-ped WHERE tt-cond-ped.nr-pedido = tt-ped-venda2.nr-pedido NO-LOCK:
            CREATE cond-ped.
            ASSIGN cond-ped.cod-vencto          = tt-cond-ped.cod-vencto  
                   cond-ped.data-pagto          = tt-cond-ped.data-pagto  
                   cond-ped.nr-dias-venc        = tt-cond-ped.nr-dias-venc
                   cond-ped.nr-pedido           = ped-venda.nr-pedido 
                   cond-ped.nr-sequencia        = tt-cond-ped.nr-sequencia 
                   cond-ped.observacoes         = tt-cond-ped.observacoes  
                   cond-ped.perc-pagto          = tt-cond-ped.perc-pagto   
                   cond-ped.vl-pagto            = tt-cond-ped.vl-pagto.     
        END. */


        FOR EACH tt-upc-ped-venda WHERE tt-upc-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
                                        tt-upc-ped-venda.nome-abrev = ped-venda.nome-abrev NO-LOCK:
            CREATE upc-ped-venda.
            ASSIGN upc-ped-venda.base-icms-iss           = tt-upc-ped-venda.base-icms-iss        
                   upc-ped-venda.base-ipi                = tt-upc-ped-venda.base-ipi             
                   upc-ped-venda.fat-par-pec             = tt-upc-ped-venda.fat-par-pec          
                   upc-ped-venda.nome-abrev              = tt-upc-ped-venda.nome-abrev           
                   upc-ped-venda.nr-pedcli               = tt-upc-ped-venda.nr-pedcli            
                   upc-ped-venda.val-tot-icms            = tt-upc-ped-venda.val-tot-icms         
                   upc-ped-venda.val-tot-ipi             = tt-upc-ped-venda.val-tot-ipi          
                   upc-ped-venda.val-tot-iss             = tt-upc-ped-venda.val-tot-iss          
                   upc-ped-venda.val-tot-ped             = tt-upc-ped-venda.val-tot-ped.          
        END.



        FOR EACH cond-pagto-contrato WHERE cond-pagto-contrato.nr-pedcli = ped-venda.nr-pedcli AND
                                        cond-pagto-contrato.nome-abrev = ped-venda.nome-abrev.
            
            ASSIGN cond-pagto-contrato.nr-pedido = 0.

            ASSIGN cond-pagto-contrato.nr-pedido             = ped-venda.nr-pedido.
        END.

        FOR EACH tt-upc-cond-ped WHERE tt-upc-cond-ped.nr-pedcli = ped-venda.nr-pedcli AND
                                        tt-upc-cond-ped.nome-abrev = ped-venda.nome-abrev NO-LOCK:

            FIND FIRST upc-cond-ped WHERE upc-cond-ped.nr-pedcli = ped-venda.nr-pedcli AND
                                          upc-cond-ped.nr-sequencia = tt-upc-cond-ped.nr-sequencia NO-ERROR.
            IF NOT AVAIL upc-cond-ped THEN DO:
                CREATE upc-cond-ped.
                ASSIGN upc-cond-ped.nr-pedcli = ped-venda.nr-pedcli 
                       upc-cond-ped.nr-sequencia = tt-upc-cond-ped.nr-sequencia
                       upc-cond-ped.dat-pre-eve           = tt-upc-cond-ped.dat-pre-eve    
                       upc-cond-ped.dat-rea-eve           = tt-upc-cond-ped.dat-rea-eve 
                       upc-cond-ped.sit                   = tt-upc-cond-ped.sit  .

            END.
            ASSIGN upc-cond-ped.cod-cond-pag          = tt-upc-cond-ped.cod-cond-pag   
                      
                   upc-cond-ped.dias-evento           = tt-upc-cond-ped.dias-evento    
                   upc-cond-ped.dsc-evento            = tt-upc-cond-ped.dsc-evento     
                   upc-cond-ped.nome-abrev            = tt-upc-cond-ped.nome-abrev     
                   upc-cond-ped.nr-pedido             = ped-venda.nr-pedido      
                   upc-cond-ped.nr-sequencia          = tt-upc-cond-ped.nr-sequencia   
                   upc-cond-ped.tipo-eve              = tt-upc-cond-ped.tipo-eve       
                   upc-cond-ped.valor                 = tt-upc-cond-ped.valor .    

        END.

        FOR EACH upc-cond-ped WHERE upc-cond-ped.nr-pedcli = ped-venda.nr-pedcli AND
                                        upc-cond-ped.nome-abrev = ped-venda.nome-abrev.
            ASSIGN upc-cond-ped.nr-pedido             = ped-venda.nr-pedido.
        END.

/*        RUN esp/esp/scp/scpinc/gera-pedido-cond-bo.p  (INPUT TABLE tt-cond-ped ,
                                                      INPUT ROWID(ped-venda) ) .

*/

        /* buffer-copy p-table  TO upc-doc-pend-aprov*/
        /* INPUT TABLE tt-ped-repre ,
                                                        INPUT TABLE tt-ped-ent ,
                                                        INPUT TABLE tt-cond-ped ,
                                                        INPUT TABLE tt-upc-ped-venda ,
                                                        INPUT TABLE tt-upc-cond-ped ,
                                                        INPUT tt-ped-venda2.nr-pedcli */



    END PROCEDURE.
