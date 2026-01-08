@echo on

rem ::set JAVA_HOME="c:\Program Files (x86)\Java\jre7"
rem set JAVA_HOME="C:\Arquivos de programas\Java\jre7"
rem set PATH=%PATH%;%JAVA_HOME%\bin

rem echo processando arquivos xml da imatextil, aguarde...
rem java -jar T:\especificos\ems2\dts\ArqConf\LoaderParamCripto.jar T:\especificos\ems2\dts\ArqConf\Arqconf_ima.cfg
rem echo.

rem echo processando arquivos xml da medtextil, aguarde...
rem java -jar T:\especificos\ems2\dts\ArqConf\LoaderParamCripto.jar T:\especificos\ems2\dts\ArqConf\Arqconf_med.cfg

set ARQCONFDIR=T:\especificos\ems2\dts\ArqConf
set
java -version

REM INICIANDO CONSULTA MEDBH ######
java -jar %ARQCONFDIR%\LoaderParamCripto.jar %ARQCONFDIR%\arqconf.cfg




exit