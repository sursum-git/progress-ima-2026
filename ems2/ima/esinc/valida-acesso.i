FIND usuar_grp_usuar WHERE
     usuar_grp_usuar.cod_usuar = c-seg-usuario AND 
     usuar_grp_usuar.cod_grp_usuar = 'SUP' NO-LOCK NO-ERROR.
IF NOT AVAIL usuar_grp_usuar THEN DO.
   MESSAGE 'Esse programa foi descontinuado. Favor utilizar a versÆo Web atrav‚s do portal de acesso'
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   OS-COMMAND SILENT VALUE('start https://imaonline.imatextil.com.br/portal_acesso_2/app_ldap_Login/?url_apl_princ=cons_imce025').
      
   RUN local-exit.
   RUN pi-after-initialize.
END.
