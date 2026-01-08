For Each ems2mov.ped-venda No-lock
            Where ems2mov.ped-venda.cod-canal-venda >= i-cod-canal-venda-ini 
            And   ems2mov.ped-venda.cod-canal-venda <= i-cod-canal-venda-fim 
            And   ems2mov.ped-venda.cod-emitente >= i-cod-emitente-ini 
            And   ems2mov.ped-venda.cod-emitente <= i-cod-emitente-fim 
            And   ems2mov.ped-venda.dt-entrega >= da-dt-entrega-ini 
            And   ems2mov.ped-venda.dt-entrega <= da-dt-entrega-fim 
            And   ems2mov.ped-venda.dt-implant >= da-dt-implant-ini 
            And   ems2mov.ped-venda.dt-implant <= da-dt-implant-fim
            And   ems2mov.ped-venda.dt-cancela = ?,
    
            Each mgrica.pedidos No-lock
                Where mgrica.pedidos.dt-entrega = ems2mov.ped-venda.dt-entrega
                And   mgrica.pedidos.cod-emitente = ems2mov.ped-venda.cod-emitente 
                And   mgrica.pedidos.nr-pedcli = ems2mov.ped-venda.nr-pedcli 
                And   mgrica.pedidos.cod-rep >= i-cod-rep-ini 
                And   mgrica.pedidos.cod-rep <= i-cod-rep-fim,
        
            Each ems2mov.ped-item No-lock
                Where ems2mov.ped-item.nome-abrev = ems2mov.ped-venda.nome-abrev 
                And   ems2mov.ped-item.nr-pedcli = ems2mov.ped-venda.nr-pedcli,
        
            Each ems2cad.emitente No-lock
                Where ems2cad.emitente.cod-emitente = ems2mov.ped-venda.cod-emitente,
        
            Each ems2cad.repres No-lock
                Where ems2cad.repres.cod-rep = ems2cad.emitente.cod-rep,
        
            Each ems2cad.loc-entr No-lock
                Where ems2cad.loc-entr.nome-abrev = ems2mov.ped-venda.nome-abrev
                And   ems2cad.loc-entr.cod-entrega = ems2mov.ped-venda.cod-entrega
        
                Break By ems2mov.ped-venda.dt-entrega
                      By ems2mov.ped-venda.cod-canal-venda  
                      By ems2mov.ped-venda.dt-implant  
                      By ems2cad.repres.cod-rep
                      By ems2cad.emitente.cod-emitente
                      :

                     If ped-venda.log-ped-bonif-pendente = Yes Then Next.
                     /*If ems2mov.ped-venda.dt-cancela <> ? Then Assign v-aux = "".*/ /*verifica‡Æo do programa (IF - de filtros)*/

    Assign v-num-reg-lidos  = v-num-reg-lidos  + 1.
           v-contador-rep   = v-contador-rep   + 1.
           v-contador-canal = v-contador-canal + 1.
           v-contador-data  = v-contador-data  + 1.

           /*Message v-aux View-as Alert-box.*/

    Run pi-acompanhar In h-acomp(Input String(String(v-num-reg-lidos) + " - " + "Dt. Processamento: " + 
                                              String(ems2mov.ped-venda.dt-entrega ) + v-aux)).

        Assign de-vl-liq-it-tt-001 = de-vl-liq-it-tt-001 + 
                                             ems2mov.ped-item.vl-liq-it
               de-vl-tot-it-tt-002 = de-vl-tot-it-tt-002 + 
                                             ems2mov.ped-item.vl-tot-it
               de-qt-atendida-tt-003 = de-qt-atendida-tt-003 + 
                                               ems2mov.ped-item.qt-atendida
               /**/
               de-vl-liq-it-tt-004 = de-vl-liq-it-tt-004 +                  
                                             ems2mov.ped-item.vl-liq-it     
               de-vl-tot-it-tt-005 = de-vl-tot-it-tt-005 +                  
                                             ems2mov.ped-item.vl-tot-it     
               de-qt-atendida-tt-006 = de-qt-atendida-tt-006 +              
                                               ems2mov.ped-item.qt-atendida
               /**/
               de-vl-liq-it-tt-007 = de-vl-liq-it-tt-007 +                  
                                             ems2mov.ped-item.vl-liq-it     
               de-vl-tot-it-tt-008 = de-vl-tot-it-tt-008 +                  
                                             ems2mov.ped-item.vl-tot-it     
               de-qt-atendida-tt-009 = de-qt-atendida-tt-009 +              
                                               ems2mov.ped-item.qt-atendida.

        CREATE tt-auxiliar. 

        ASSIGN tt-auxiliar.dt-entrega        = ped-venda.dt-entrega
               tt-auxiliar.cod-canal-venda   = ped-venda.cod-canal-venda
               tt-auxiliar.cod-repres        = repres.cod-rep
               tt-auxiliar.cod-emitente      = ped-venda.cod-emitente   
               tt-auxiliar.data-implant      = ped-venda.dt-implant     
               tt-auxiliar.valor-liq         = ped-item.vl-liq-it       
               tt-auxiliar.valor-total       = ped-item.vl-tot-it       
               tt-auxiliar.qtde-atende       = ped-item.qt-atendida     
               tt-auxiliar.endereco          = loc-entr.endereco        .


/*         IF LAST-OF(repres.cod-rep) /*OR LAST-OF(repres.cod-rep)*/ /* or (i-cont-itens = 85 AND NOT LAST-OF(emitente.cod-canal-venda) */ THEN DO:                    */
/*                                                                                                                                                                     */
/*             CREATE tt-auxiliar.                                                                                                                                     */
/*                                                                                                                                                                     */
/*             ASSIGN tt-auxiliar.cod-canal-venda   = 0                                                                                                                */
/*                    tt-auxiliar.cod-rep           = mgrica.pedidos.cod-rep                                                                                           */
/*                    tt-auxiliar.cod-emitente      = 0                                                                                                                */
/*                    tt-auxiliar.data-implant      = ?                                                                                                                */
/*                    tt-auxiliar.valor-liq         = 0                                                                                                                */
/*                    tt-auxiliar.valor-total       = 0                                                                                                                */
/*                    tt-auxiliar.qtde-atende       = 0                                                                                                                */
/*                    tt-auxiliar.endereco          = ""                                                                                                               */
/*                    de-vl-liq-it-tt-001           = IF LAST-OF(repres.cod-rep) /*OR LAST-OF(repres.cod-rep)*/ THEN 0 ELSE de-vl-liq-it-tt-001                        */
/*                    de-vl-tot-it-tt-002           = de-vl-tot-it-tt-002                                                                                              */
/*                    de-qt-atendida-tt-003         = de-qt-atendida-tt-003                                                                                            */
/*     .                                                                                                                                                               */
/*                                                                                                                                                                     */
/*        IF LAST-OF(ems2mov.ped-venda.cod-canal-venda) /*OR LAST-OF(repres.cod-rep)*/ /* or (i-cont-itens = 85 AND NOT LAST-OF(emitente.cod-canal-venda) */ THEN DO:  */
/*                                                                                                                                                                     */
/*            CREATE tt-auxiliar.                                                                                                                                      */
/*                                                                                                                                                                     */
/*            ASSIGN tt-auxiliar.cod-canal-venda   = ped-venda.cod-canal-venda                                                                                         */
/*                   tt-auxiliar.cod-rep           = 0                                                                                                                 */
/*                   tt-auxiliar.cod-emitente      = 0                                                                                                                 */
/*                   tt-auxiliar.data-implant      = ?                                                                                                                 */
/*                   tt-auxiliar.valor-liq         = 0                                                                                                                 */
/*                   tt-auxiliar.valor-total       = 0                                                                                                                 */
/*                   tt-auxiliar.qtde-atende       = 0                                                                                                                 */
/*                   tt-auxiliar.endereco          = ""                                                                                                                */
/*                   de-vl-liq-it-tt-004           = IF LAST-OF(repres.cod-rep) /*OR LAST-OF(repres.cod-rep)*/ THEN 0 ELSE de-vl-liq-it-tt-004                         */
/*                   de-vl-tot-it-tt-005           = de-vl-tot-it-tt-005                                                                                               */
/*                   de-qt-atendida-tt-006         = de-qt-atendida-tt-006                                                                                             */
/*     .                                                                                                                                                               */
/*         IF LAST-OF(ped-venda.dt-entrega) /*OR LAST-OF(emitente.cod-canal-venda)*/ /* or (i-cont-itens = 85 AND NOT LAST-OF(emitente.cod-canal-venda) */ THEN DO:    */
/*                                                                                                                                                                     */
/*             CREATE tt-auxiliar.                                                                                                                                     */
/*                                                                                                                                                                     */
/*             ASSIGN tt-auxiliar.cod-canal-venda   = 0                                                                                                                */
/*                    tt-auxiliar.cod-rep           = 0                                                                                                                */
/*                    tt-auxiliar.cod-emitente      = 0                                                                                                                */
/*                    tt-auxiliar.data-implant      = ped-venda.dt-implant                                                                                             */
/*                    tt-auxiliar.valor-liq         = 0                                                                                                                */
/*                    tt-auxiliar.valor-total       = 0                                                                                                                */
/*                    tt-auxiliar.qtde-atende       = 0                                                                                                                */
/*                    tt-auxiliar.endereco          = ""                                                                                                               */
/*                    de-vl-liq-it-tt-007           = IF LAST-OF(repres.cod-rep) /*OR LAST-OF(repres.cod-rep)*/ THEN 0 ELSE de-vl-liq-it-tt-007                        */
/*                    de-vl-tot-it-tt-008           = de-vl-tot-it-tt-008                                                                                              */
/*                    de-qt-atendida-tt-009         = de-qt-atendida-tt-009                                                                                            */
/*              .                                                                                                                                                      */
/*           END.                                                                                                                                                      */
/*          END.                                                                                                                                                       */
/*     END.                                                                                                                                                            */
END.

{PDFInclude\pdf_inc.i}

        RUN pi-inicializar in h-acomp(input USERID("ems5") + "," + " " + "Aguarde" + "." + " ." + "  ." ).

        RUN pdf_new("Spdf","c:\Temp\rpt-pdf\exercicio_12.pdf"). 
        RUN pdf_set_parameter("Spdf","Compress","True").
        
        pdf_PageHeader ("Spdf", 
                        THIS-PROCEDURE:HANDLE, 
                        "PageHeader"). 
        
        pdf_PageFooter ("Spdf", 
                        THIS-PROCEDURE:HANDLE, 
                        "PageFooter"). 
        
        RUN pdf_set_LeftMargin      ("Spdf",20). /*Margem*/ 
        RUN pdf_set_BottomMargin    ("Spdf",50). /*Margem*/ 
        
        h_TT = TEMP-TABLE tt-auxiliar:HANDLE. /*pega o handle da TT*/ 
        
        RUN pi-inicializar in h-acomp(input USERID("ems5") + "," + " " + "Definindo parametros...").

        RUN pdf_tool_add          ("Spdf","auxiliar","TABLE"          , h_TT).                    /*carrega a temp-tabla na Lista*/ 

        RUN pdf_set_tool_parameter("Spdf","auxiliar","MaxY",0,10).
        RUN pdf_set_tool_parameter("Spdf","auxiliar","MaxX",0,10).

        RUN pdf_set_Orientation ("Spdf","Landscape").
        /**/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","Outline"        ,0,".1").                     /*Tamanho das bordas externas*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","CellUnderline"  ,0,".1").                     /*Tamanho das bordas internas*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","HeaderFont"     ,0,"Helvetica-Bold").         /*Fonte do cabeçalho*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","HeaderFontSize" ,0,"7").                      /*Tamanho da Fonte do cabeçalho*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","HeaderBGColor"  ,0,"0,0,0").                  /*Cor do Fundo do Cabeçalho*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","HeaderTextColor",0,"255,255,255").            /*Cor do Texto do Titulo*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","DetailTextColor",0,"0,0,0" ).                 /*Cor do Texto*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","DetailFont"     ,0,"Helvetica").              /*Estilo da Fonte*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","DetailFontSize" ,0,"6").                      /*Tamanho da Fonte*/
        /**/                                  
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,1,"Dt.Entrega").             /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,2,"Canal Vendas").           /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,3,"Representante").          /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,4,"Emitente").               /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,5,"Dt.Implant.").            /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,6,"Valor L¡quido").          /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,7,"Valor Total").            /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,8,"Quantidade").             /*Cabe‡alho da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnHeader"   ,9,"Endere‡o").               /*Cabe‡alho da coluna*/
        /**/                                                                                    
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,1,"10").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,2,"10").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,3,"10").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,4,"12").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,5,"10").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,6,"15").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,7,"15").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,8,"15").                     /*Item da coluna*/
        RUN pdf_set_tool_parameter("Spdf","auxiliar","ColumnWidth"    ,9,"25").                     /*Item da coluna*/
        /**/
        RUN pdf_tool_create         ("Spdf","auxiliar"). /*Cria a tabela no PDF*/ 
/*         MESSAGE "entrei close" VIEW-AS ALERT-BOX INFORMATION TITLE "aviso".  */
        RUN pdf_close("Spdf").
/*         MESSAGE "sai close" VIEW-AS ALERT-BOX INFORMATION TITLE "aviso". */

        run pi-inicializar in h-acomp(input USERID("ems5") + "," + " " + "Definindo Cabe‡alho e Rodap‚...").

        PROCEDURE PageHeader: 

          RUN pdf_set_font          ("Spdf","Courier-Bold",10.0).  
          RUN pdf_text_color        ("Spdf",.0,.0,.0). 
          RUN pdf_text_xy           ("Spdf","Relatorio Estatistico de Compradores" + "              " + "Date: " +
                                    String(DATE(TODAY)) + "              " + "Usu rio: " +
                                    STRING(USERID("ems5")),pdf_TopMargin("Spdf"),pdf_PageHeight("Spdf") - 25).
          RUN pdf_skip              ("spdf").
          RUN pdf_text_xy           ("Spdf","______________________________________________________________________________________________________"
                                      ,pdf_TopMargin("Spdf"),pdf_PageHeight("Spdf") - 35).

          RUN pdf_text_color        ("Spdf",.0,.0,.0). 
          RUN pdf_set_font          ("Spdf","Courier",10.0).                                  

        END. 

        PROCEDURE PageFooter: 

          RUN pdf_set_font          ("Spdf","Courier-Bold",8.0).  
          RUN pdf_text_color        ("Spdf",0.0,.0,.0). 
          RUN pdf_skip              ("Spdf"). 
          RUN pdf_set_dash          ("Spdf",3,0). 
          RUN pdf_line              ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1). 
          RUN pdf_skip              ("Spdf"). 
          RUN pdf_skip              ("Spdf"). 
          RUN pdf_text_to           ("Spdf",  "Page: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 171). 

        END. 


          RUN pi-inicializar in h-acomp(input USERID("ems5") + "," + " " + "Arquivo criado com sucesso !").

          run winexec (input "explorer  c:\temp\rpt-pdf", input 1 ). 

            procedure WinExec external "kernel32.dll":u: 
                def input parameter pProgName as char. 
                def input parameter pStyle as long. 
            end procedure.
