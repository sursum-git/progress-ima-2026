disable triggers for load of ems2ima.emitente.
disable triggers for load of ems2med.emitente.
disable triggers for load of ems5.clien_financ.

define variable c-saida      as character no-undo.
DEFINE VARIABLE d-dt-implant AS DATE INITIAL TODAY NO-UNDO.
/*DEFINE VARIABLE d-dt-implant AS DATE INITIAL 03.12.2015 NO-UNDO. */

DEFINE VARIABLE cod-rep-ima    AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE cod-rep-med    AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE cdn_repres-100 AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE cdn_repres-500 AS INT  INITIAL 0 NO-UNDO.

DEF TEMP-TABLE tt-ped-venda
    FIELD cod-estabel  LIKE ems2ima.ped-venda.cod-estabel
    FIELD cod-emitente LIKE ems2ima.ped-venda.cod-emitente
    FIELD no-ab-reppri LIKE ems2ima.ped-venda.no-ab-reppri
    FIELD nr-pedcli    LIKE ems2ima.ped-venda.nr-pedcli
    FIELD dt-implant   LIKE ems2ima.ped-venda.dt-implant.

assign c-saida = if opsys = "win32" then "c:\temp\" else "/log/ped-venda/".
assign c-saida = c-saida + "prgvalida_ped-venda_" + string(today,"99-99-99") + "_" + replace(string(time,"hh:mm"),":","h") + ".csv".

os-delete value(c-saida).
output to value(c-saida).

put UNFORMAT "ped-venda.cod-estabel;ped-venda.cod-emitente;ped-venda.no-ab-reppri;ped-venda.nr-pedcli;ped-venda.dt-implant;emitente.cod-emitente;emitente.nome-abrev;emitente.cod-rep;cod-rep-ima;cod-rep-med;repres.cod-rep;repres.nome-abrev;clien_financ.cod_empresa;clien_financ.cdn_cliente;clien_financ.cdn_repres;cdn_repres-100;cdn_repres-500" SKIP.

for each ems2ima.ped-venda no-lock where 
         ems2ima.ped-venda.no-ab-reppri <> "ima"
     and ems2ima.ped-venda.cod-emitente <> 1
     and ems2ima.ped-venda.dt-implant   = d-dt-implant.

    CREATE tt-ped-venda.
    ASSIGN tt-ped-venda.cod-estabel  = ems2ima.ped-venda.cod-estabel 
           tt-ped-venda.cod-emitente = ems2ima.ped-venda.cod-emitente
           tt-ped-venda.no-ab-reppri = ems2ima.ped-venda.no-ab-reppri
           tt-ped-venda.nr-pedcli    = ems2ima.ped-venda.nr-pedcli   
           tt-ped-venda.dt-implant   = ems2ima.ped-venda.dt-implant.
END.

for each ems2med.ped-venda no-lock where 
         ems2med.ped-venda.no-ab-reppri <> "ima"
     and ems2med.ped-venda.cod-emitente <> 1
     and ems2med.ped-venda.dt-implant   = d-dt-implant.

    CREATE tt-ped-venda.
    ASSIGN tt-ped-venda.cod-estabel  = ems2med.ped-venda.cod-estabel 
           tt-ped-venda.cod-emitente = ems2med.ped-venda.cod-emitente
           tt-ped-venda.no-ab-reppri = ems2med.ped-venda.no-ab-reppri
           tt-ped-venda.nr-pedcli    = ems2med.ped-venda.nr-pedcli   
           tt-ped-venda.dt-implant   = ems2med.ped-venda.dt-implant.
END.

FOR EACH tt-ped-venda,

    each ems2ima.emitente EXCLUSIVE-LOCK where
         ems2ima.emitente.cod-emitente = tt-ped-venda.cod-emitente,

    each ems2med.emitente EXCLUSIVE-LOCK where
         ems2med.emitente.cod-emitente = tt-ped-venda.cod-emitente,
		 
    each ems2ima.repres NO-LOCK where
         ems2ima.repres.nome-abrev = tt-ped-venda.no-ab-reppri,

    each ems2med.repres NO-LOCK where
         ems2med.repres.nome-abrev = tt-ped-venda.no-ab-reppri.

    FOR EACH ems5.clien_financ EXCLUSIVE-LOCK where
             ems5.clien_financ.cdn_cliente = tt-ped-venda.cod-emitente
        AND (ems5.clien_financ.cod_empresa = "100"
          OR ems5.clien_financ.cod_empresa = "500").

        if ems2ima.emitente.cod-rep = 1 OR ems2med.emitente.cod-rep = 1 OR ems5.clien_financ.cdn_repres = 1 THEN DO:

           ASSIGN cod-rep-ima    = 0
                  cod-rep-med    = 0
                  cdn_repres-100 = 0
                  cdn_repres-500 = 0.

           IF ems5.clien_financ.cod_empresa = "100" THEN
              ASSIGN cdn_repres-100               = ems5.clien_financ.cdn_repres
                     cod-rep-ima                  = ems2ima.emitente.cod-rep
                     ems2ima.emitente.cod-rep     = ems2ima.repres.cod-rep
                     ems5.clien_financ.cdn_repres = ems2ima.repres.cod-rep.

           IF ems5.clien_financ.cod_empresa = "500" THEN
              ASSIGN cdn_repres-500               = ems5.clien_financ.cdn_repres
                     cod-rep-med                  = ems2med.emitente.cod-rep
                     ems2med.emitente.cod-rep     = ems2med.repres.cod-rep
                     ems5.clien_financ.cdn_repres = ems2med.repres.cod-rep.

           put UNFORMAT
                  tt-ped-venda.cod-estabel
               ";"tt-ped-venda.cod-emitente
               ";"tt-ped-venda.no-ab-reppri
               ";"tt-ped-venda.nr-pedcli
               ";"tt-ped-venda.dt-implant
               ";"ems2ima.emitente.cod-emitente
               ";"ems2ima.emitente.nome-abrev
               ";"ems2ima.emitente.cod-rep
               ";"cod-rep-ima
               ";"cod-rep-med
               ";"ems2ima.repres.cod-rep
               ";"ems2ima.repres.nome-abrev
               ";"ems5.clien_financ.cod_empresa
               ";"ems5.clien_financ.cdn_cliente
               ";"ems5.clien_financ.cdn_repres
               ";"cdn_repres-100
               ";"cdn_repres-500
               skip.
        END.
    END.
END.

output close.
