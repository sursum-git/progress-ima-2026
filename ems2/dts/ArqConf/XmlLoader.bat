set ARQCONFDIR=\\servidor-db\producao\ERP\esp\Xmlloader\ArqConf
#set ARQCONFDIR=\\hperpapp\integracao\xmloader\ArqConf
set
java -version
java -jar %ARQCONFDIR%\LoaderParamCripto.jar %ARQCONFDIR%\arqconf.cfg
pause.

exit.








