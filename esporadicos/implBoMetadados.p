 DEFINE VARIABLE hBo AS HANDLE         NO-UNDO.
 DEFINE VARIABLE cBanco AS CHARACTER   NO-UNDO.
 {esbo/boMetadados.i}
 RUN esbo/boMetaDados.p PERSISTENT SET hBo .
 //RUN setBanco IN hBo('ems5').
 RUN setTabela IN hBo('tit_acr').
 RUN getCpsTb IN hBo.
 RUN getTTCps IN hBO(OUTPUT TABLE ttCampos).
 RUN getBanco IN hBo(OUTPUT cBanco).
 MESSAGE cBanco
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
 FOR EACH ttCampos
     WHERE ttCampos.nome = 'ind_orig_tit_acr':
     DISP ttCampos.labelcampo
          substr(ttCampos.lista,1,100) FORMAT 'x(100)' LABEL "lista"     
          //substr(ttCampos.lista,1,100) FORMAT 'x(100)'
          WITH WIDTH 550 . 
 END.
