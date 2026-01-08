
disable triggers for load of ems2ima.emitente.
disable triggers for load of ems2med.emitente.
disable triggers for load of ems5.clien_financ.

define variable c-saida         as character                  no-undo.
define variable l-flag          as logical initial false      no-undo.

define variable d-ultdata-ima   as date initial 01.01.1000    no-undo.
define variable c-ultpedcli-ima as character initial ""       no-undo.

define variable d-ultdata-med   as date initial 01.01.1000    no-undo.
define variable c-ultpedcli-med as character initial ""       no-undo.

define variable i-qtddias-ima   as integer initial 0          no-undo.
define variable i-qtddias-med   as integer initial 0          no-undo.
define variable i-qtddias-cli   as integer initial 0          no-undo.

ASSIGN c-saida = IF OPSYS = "WIN32" THEN "c:\temp\" ELSE "/log/repres/".
ASSIGN c-saida = c-saida + "prgvalida_repres_" + string(today,"99-99-99") + "_" + replace(string(time,"hh:mm"),":","h") + ".csv".

os-delete value(c-saida).
output to value(c-saida).

find espec.im-param where
     espec.im-param.cod-param = "valida_repres"
     no-lock no-error.

if not avail espec.im-param then do:
   put unformat "parametro nao encontrado na tabela espec.im-param".
   leave.
end.

put unformat "relatorio diario valida cliente do representante mais de " + espec.im-param.val-param + " dias" skip.
put unformat "ems2ima.repres;;;ems2ima.emitente;;;;;imatextil;;;;medtextil" skip.
put unformat "cod-rep;nome-abrev;;cod-emitente;nome-abrev;dt-implant;i-qtddias-cli;;d-ultdata-ima;c-ultpedcli-ima;i-qtddias-ima;;d-ultdata-med;c-ultpedcli-med;i-qtddias-med" skip.

for each ems2ima.repres where
         ems2ima.repres.cod-rep <> 1 and
         ems2ima.repres.cod-rep <> 2
         no-lock,

    each ems2ima.emitente where
         ems2ima.emitente.cod-rep = ems2ima.repres.cod-rep
         share-lock,

    each ems2med.emitente where
         ems2med.emitente.cod-emitente = ems2ima.emitente.cod-emitente
         share-lock.

    for each ems2ima.ped-venda no-lock where
             ems2ima.ped-venda.no-ab-reppri = ems2ima.repres.nome-abrev and
             ems2ima.ped-venda.nome-abrev   = ems2ima.emitente.nome-abrev
        use-index ch-rep-cli.

        if ems2ima.ped-venda.dt-emissao >= d-ultdata-ima then do:
           assign d-ultdata-ima = ems2ima.ped-venda.dt-emissao.
           if int(ems2ima.ped-venda.nr-pedcli) > int(c-ultpedcli-ima) then assign c-ultpedcli-ima = ems2ima.ped-venda.nr-pedcli.
        end.
    end.

    for each ems2med.ped-venda no-lock where
             ems2med.ped-venda.no-ab-reppri = ems2ima.repres.nome-abrev and
             ems2med.ped-venda.nome-abrev   = ems2med.emitente.nome-abrev
        use-index ch-rep-cli.

        if ems2med.ped-venda.dt-emissao >= d-ultdata-med then do:
           assign d-ultdata-med = ems2med.ped-venda.dt-emissao.
           if ems2med.ped-venda.nr-pedcli > c-ultpedcli-med then assign c-ultpedcli-med = ems2med.ped-venda.nr-pedcli.
        end.
    end.

    assign i-qtddias-cli = today - ems2ima.emitente.data-implant.
    if c-ultpedcli-ima <> "" then assign i-qtddias-ima = today - d-ultdata-ima.
    if c-ultpedcli-med <> "" then assign i-qtddias-med = today - d-ultdata-med.

    assign l-flag = false.
    if (c-ultpedcli-ima = "" and c-ultpedcli-med = "") then
       assign l-flag = true.
       if i-qtddias-cli < int(espec.im-param.val-param) then assign l-flag = false.
    else do:
       if (i-qtddias-ima > int(espec.im-param.val-param) and i-qtddias-med > int(espec.im-param.val-param)) then assign l-flag = true.
       if (c-ultpedcli-ima = "" and i-qtddias-med > int(espec.im-param.val-param)) then assign l-flag = true.
       if (c-ultpedcli-med = "" and i-qtddias-ima > int(espec.im-param.val-param)) then assign l-flag = true.
    end.

    if l-flag then do:
       put unformat ems2ima.repres.cod-rep ";" ems2ima.repres.nome-abrev ";;" ems2ima.emitente.cod-emitente ";" ems2ima.emitente.nome-abrev ";" ems2ima.emitente.data-implant ";" i-qtddias-cli ";;" d-ultdata-ima ";" c-ultpedcli-ima ";" i-qtddias-ima ";;" d-ultdata-med ";" c-ultpedcli-med ";" i-qtddias-med skip.
       assign ems2ima.emitente.cod-rep = 1
              ems2med.emitente.cod-rep = 1.

       for each ems5.clien_financ where
                ems5.clien_financ.cdn_cliente = ems2ima.emitente.cod-emitente and
                ems5.clien_financ.cdn_repres  = ems2ima.repres.cod-rep
                share-lock.
           assign ems5.clien_financ.cdn_repres = 1.
       end.
    end.

    assign d-ultdata-ima   = 01.01.1000
           d-ultdata-med   = 01.01.1000
           c-ultpedcli-ima = ""
           c-ultpedcli-med = ""
           i-qtddias-ima   = 0
           i-qtddias-med   = 0
           i-qtddias-cli   = 0
           l-flag          = false.

end.

output close.
