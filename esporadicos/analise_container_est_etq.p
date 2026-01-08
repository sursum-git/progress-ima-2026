{esp/util.i}

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD itCodigo    LIKE ITEM.it-codigo
    FIELD codRefer    AS CHAR
    FIELD nrcontainer AS DECIMAL
    FIELD nrDocto     AS CHAR
    FIELD qtVendida   AS DECIMAL
    FIELD qtEtqs      AS INT
    FIELD tipo        AS CHAR
    INDEX ind-item IS PRIMARY itCodigo codRefer nrContainer nrDocto.
    
    
DEFINE VARIABLE dtIniContainer AS DATE        NO-UNDO.
DEFINE VARIABLE nrDoctoIni     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dtFimContainer AS DATE        NO-UNDO.

DEFINE VARIABLE iContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtDias   AS INTEGER     NO-UNDO INIT 999999.


UPDATE iContainer qtDias. //295324



RUN utp/ut-acomp.p PERSIST SET h-acomp.

RUN pi-inicializar IN h-acomp('Buscando Movtos').

FOR EACH pp-container NO-LOCK
    WHERE pp-container.nr-container = iContainer :
    FIND nfs_container NO-LOCK
    WHERE nfs_container.container_id = pp-container.nr-container  NO-ERROR.
    IF AVAIL nfs_container THEN DO:
       FOR FIRST docum-est fields(dt-trans nro-docto  serie-docto cod-emitente)
       NO-LOCK
       WHERE docum-est.serie-docto = nfs_container.serie
       AND   docum-est.nro-docto   = nfs_container.documento:       
       ASSIGN dtIniContainer = docum-est.dt-trans
              nrDoctoIni     = docum-est.nro-docto  
              dtFimContainer = dtIniContainer + qtDias.            
       END.       
    END.
    
    FOR EACH pp-it-container OF pp-container NO-LOCK:       
        FOR EACH movto-estoq FIELDS( cod-estabel serie-docto  nro-docto sequen-nf dt-trans it-codigo cod-refer cod-depos nat-operacao tipo-trans esp-docto quantidade) NO-LOCK
            WHERE movto-estoq.it-codigo = pp-it-container.it-codigo 
            AND   movto-estoq.cod-refer = pp-it-container.cod-refer
            AND   movto-estoq.dt-trans >= dtIniContainer
            AND   movto-estoq.dt-trans <= dtFimContainer
            USE-INDEX data-item:
            RUN pi-acompanhar IN h-acomp('Item:' + movto-estoq.it-codigo + ' - Ref.' + movto-estoq.cod-refer + ' - data:' + string(movto-estoq.dt-trans) + "| " + movto-estoq.nro-docto ).
            IF movto-estoq.esp-docto = 22 THEN DO:
               FOR FIRST it-nota-fisc 
               FIELDS(nr-nota-fis cod-estabel serie nat-operacao dt-emis-nota nr-pedcli nome-ab-cli nr-seq-ped it-codigo cod-refer nr-seq-fat)                
               WHERE it-nota-fisc.cod-estabel   = movto-estoq.cod-estabel
               AND   it-nota-fisc.serie         = movto-estoq.serie-docto
               AND   it-nota-fisc.nr-nota-fis   = movto-estoq.nro-docto
               AND   it-nota-fisc.nr-seq-fat    = movto-estoq.sequen-nf 
               AND   it-nota-fisc.it-codigo     = movto-estoq.it-codigo NO-LOCK  USE-INDEX ch-nota-item.
               FIND ttItem
                    WHERE ttItem.itCodigo     = it-nota-fisc.it-codigo
                    AND   ttitem.codRefer     = it-nota-fisc.cod-refer
                    AND   ttItem.nrContainer  = 0
                    AND   ttitem.nrdocto      = movto-estoq.nro-docto NO-ERROR.
                    IF NOT AVAIL ttItem THEN DO:
                       CREATE ttItem.
                       ASSIGN ttItem.itCodigo     = it-nota-fisc.it-codigo
                              ttItem.codRefer     = it-nota-fisc.cod-refer 
                              ttItem.nrContainer  = 0
                              ttItem.nrDocto      = movto-estoq.nro-docto 
                              ttItem.qtVendida    = movto-estoq.quantidade
                              ttItem.tipo         = 'movto'
                              .                                        
                    END.                                   
               END.                
               IF AVAIL it-nota-fisc THEN DO:
                  RUN pi-acompanhar IN h-acomp('NF Item:' + movto-estoq.it-codigo + ' - Ref.' + movto-estoq.cod-refer + ' - data:' + string(movto-estoq.dt-trans) + "| " + movto-estoq.nro-docto ).
                  FOR FIRST ped-item 
                      WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
                      AND   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
                      AND   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped 
                      AND   ped-item.it-codigo    = it-nota-fisc.it-codigo
                      AND   ped-item.cod-refer    = it-nota-fisc.cod-refer USE-INDEX ch-item-ped:
                      OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + 'etqs_NF' + it-nota-fisc.nr-nota-fis + '-SEQNF'  + string(it-nota-fisc.nr-seq-fat) + '-PED'  + ped-item.nr-pedcli + '-SEQPED' + string(ped-item.nr-sequencia) + '.csv') .
                      PUT "Etiqueta;Item;Refer;Qte" SKIP.
                      FOR EACH ped-item-rom 
                        WHERE  ped-item-rom.nome-abrev   = ped-item.nome-abrev
                        AND   ped-item-rom.nr-pedcli     = ped-item.nr-pedcli
                        AND   ped-item-rom.nr-sequencia  = ped-item.nr-sequencia NO-LOCK :
                        //RUN pi-acompanhar IN h-acomp('ROM Item:' + movto-estoq.it-codigo + ' - Ref.' + movto-estoq.cod-refer + ' - data:' + string(movto-estoq.dt-trans) ).
                              FOR FIRST ob-etiqueta FIELDS(nr-container quantidade num-etiqueta it-codigo cod-refer)
                              WHERE  ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel
                              AND    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta                               
                              AND ob-etiqueta.situacao = 5 NO-LOCK :
                              END.
                              IF NOT AVAIL ob-etiqueta  THEN NEXT.
                              EXPORT DELIMITER  ";" ob-etiqueta.num-etiqueta ob-etiqueta.it-codigo ob-etiqueta.cod-refer ob-etiqueta.quantidade  .
                              FIND ttItem
                              WHERE ttItem.itCodigo     = ped-item.it-codigo
                              AND   ttitem.codRefer     = ped-item.cod-refer
                              AND   ttItem.nrContainer  = ob-etiqueta.nr-container 
                              AND   ttitem.nrdocto      = movto-estoq.nro-docto NO-ERROR.
                              IF NOT AVAIL ttItem THEN DO:
                                 CREATE ttItem.
                                 ASSIGN ttItem.itCodigo     = ped-item.it-codigo
                                        ttItem.codRefer     = ped-item.cod-refer 
                                        ttItem.nrContainer  = ob-etiqueta.nr-container 
                                        ttItem.nrDocto      = movto-estoq.nro-docto 
                                        ttItem.tipo         = 'container'
                                        .                                        
                              END.                        
                              ASSIGN ttItem.qtVendida = ttItem.qtVendida + ob-etiqueta.quantidade * -1
                                     ttItem.qtEtq     = ttItem.qtEtq     + 1  .
                      END.   
                      OUTPUT CLOSE.
                  END.                   
               END.
            END.                 
        END.                
    END.
END.
RUN pi-finalizar IN h-acomp.
{esp/exportarTabelacsv3.i ttItem " " " " "  "ttItem" }


/*
FOR EACH pp-container NO-LOCK
    WHERE pp-container.nr-container = 295324:
    FIND nfs_container NO-LOCK
    WHERE nfs_container.container_id = pp-container.nr-container
    NO-ERROR.
    IF AVAIL nfs_container THEN DO:
       FOR FIRST docum-est fields(dt-trans nro-docto  serie-docto cod-emitente)
       NO-LOCK
       WHERE docum-est.serie-docto = nfs_container.serie
       AND   docum-est.nro-docto   = nfs_container.documento
       :       
       ASSIGN dtIniContainer = docum-est.dt-trans
              nrDoctoIni     = docum-est.nro-docto              .            
       END.       
    END.
    
    FOR EACH pp-it-container OF pp-container NO-LOCK:    
       
        FOR FIRST item-doc-est fields(nro-docto  serie-docto cod-emitente)
        WHERE item-doc-est.it-codigo = pp-it-container.it-codigo,
        EACH docum-est FIELDS(dt-trans) OF item-doc-est
        WHERE  docum-est.dt-trans > dtIniContainer
        AND    docum-est.nro-docto <> nrDoctoIni
        AND    docum-est.nat-operacao = '31201m':
          ASSIGN dtFimContainer = docum-est.dt-trans. 
        END.                                                 
        
        FOR EACH ob-etiqueta NO-LOCK        
        WHERE ob-etiqueta.nr-container = pp-container.nr-container
        AND ob-etiqueta.situacao = 5:
            FIND ttitem
            WHERE ttitem.itCodigo = ob-etiqueta.it-codigo NO-ERROR.
            IF NOT AVAIL ttItem THEN DO:
               CREATE ttItem.
               ASSIGN ttitem.itCodigo = pp-it-container.it-codigo                      
                      .                    
            END.
            ASSIGN qtVendida = qtVendida + ob-etiqueta.quantidade.            
        END.        
    END.
END.



FOR EACH ttItem:
    DISP ttItem WITH WIDTH 550.
END.
*/
