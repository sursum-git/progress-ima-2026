DEFINE TEMP-TABLE ttPagtos
    FIELD numIdPagto AS INT
    FIELD dtPagto    AS DATE
    FIELD codFornec  AS INT
    FIELD titulo     AS CHAR FORMAT 'x(12)'
    FIELD parcela    AS CHAR 
    FIELD serie      AS CHAR
    FIELD qtPedido   AS INT
    FIELD qtContainer AS INT
    FIELD DESC_alocacao_pedido AS CHAR FORMAT 'x(30)'
    FIELD DESC_alocacao_container AS CHAR FORMAT 'x(30)'.
FOR EACH pagtos_proc_compra.
    FIND FIRST ttPagtos
        WHERE ttPagtos.numIdPagto = pagtos_proc_compra.num_id_tit_ap
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttPagtos THEN DO:
       CREATE ttPagtos.
       FIND FIRST tit_ap
           WHERE tit_ap.cod_estab = '501'
           AND tit_ap.num_id_tit_ap = pagtos_proc_compra.num_id_tit_ap
           NO-LOCK NO-ERROR.
       ASSIGN ttPagtos.numIdPagto   = pagtos_proc_compra.num_id_tit_ap
              ttPagtos.dtPagto      = IF AVAIL tit_ap THEN tit_ap.dat_emis_docto ELSE ?
              ttPagtos.codFornec    = IF AVAIL tit_ap THEN tit_ap.cdn_fornecedor ELSE 0
              ttPagtos.titulo       = IF AVAIL tit_ap THEN tit_ap.cod_tit_ap ELSE ''
              ttPagtos.parcela      = IF AVAIL tit_ap THEN tit_ap.cod_parcela ELSE ''
              ttpagtos.serie        = IF AVAIL tit_ap THEN tit_ap.cod_ser_docto ELSE ''.
    END.

    ASSIGN ttPagtos.qtPedido = ttPagtos.qtPedido + 1
           ttPagtos.DESC_alocacao_pedido = ttPagtos.DESC_alocacao_pedido + ',' + string(pagtos_proc_compra.Proc_compra_id).
END.


FOR EACH pagtos_container.
    FIND FIRST ttPagtos
        WHERE ttPagtos.numIdPagto = pagtos_container.num_id_tit_ap
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttPagtos THEN DO:
       FIND FIRST tit_ap
           WHERE tit_ap.cod_estab = '501'
           AND tit_ap.num_id_tit_ap = pagtos_container.num_id_tit_ap
           NO-LOCK NO-ERROR.
       CREATE ttPagtos.
       ASSIGN ttPagtos.numIdPagto   = pagtos_container.num_id_tit_ap
              ttPagtos.dtPagto      = IF AVAIL tit_ap THEN tit_ap.dat_emis_docto ELSE ?
              ttPagtos.codFornec    = IF AVAIL tit_ap THEN tit_ap.cdn_fornecedor ELSE 0
              ttPagtos.titulo       = IF AVAIL tit_ap THEN tit_ap.cod_tit_ap ELSE ''   
              ttPagtos.parcela      = IF AVAIL tit_ap THEN tit_ap.cod_parcela ELSE ''  
              ttpagtos.serie        = IF AVAIL tit_ap THEN tit_ap.cod_ser_docto ELSE ''.
    END.

    ASSIGN ttPagtos.qtcontainer  = ttPagtos.qtContainer + 1
           ttPagtos.DESC_alocacao_container = ttPagtos.DESC_alocacao_container + ',' + string(pagtos_container.container_id).
END.

OUTPUT TO c:\temp\pgtos.txt.
FOR EACH ttPagtos:
    EXPORT DELIMITER "|" ttPagtos. 
END.
OUTPUT CLOSE.
