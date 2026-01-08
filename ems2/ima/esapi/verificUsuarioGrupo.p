DEFINE INPUT  PARAMETER pGrupo   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pUsuario AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lAchou   AS LOGICAL     NO-UNDO.
FIND usuar_grp_usuar
    WHERE  usuar_grp_usuar.cod_grp_usuar = pGrupo
    AND  usuar_grp_usuar.cod_usuario = pUsuario NO-LOCK NO-ERROR.

ASSIGN lAChou = AVAIL usuar_grp_usuar .

