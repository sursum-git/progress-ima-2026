@ECHO OFF

:INICIO
CLS
ECHO.
ECHO.
ECHO *************************************************************
ECHO.
ECHO WebService dts.XMLLoader
ECHO Copyright 2012 DTS Consultoria LTDA.
ECHO.
ECHO *************************************************************
ECHO.

:TESTAINTERVALO
ECHO. | TIME | FIND /I "CURRENT"
REM Informe abaixo os intervalos na sintaxe <:mm:0>, onde mm = minutos.
REM Foi necessário colocar ":" para identificar a coluna minutos
REM Foi necessário colocar ":0" para não ficar testando os e-mails todos os 60 segundos do minuto.
FOR %%X IN (":00:0" ":10:0" ":20:0" ":30:0" ":40:0" ":50:0") DO (
REM ECHO %%X
ECHO. | TIME | FIND /I %%X
if NOT ERRORLEVEL==1 GOTO LER
)

GOTO TESTAINTERVALO

:LER
ECHO Lendo e-mails...
"C:\Program Files\Java\jre7\bin\java.exe" -jar M:\EMS206\especificos\xmlloader\ArqConf\Loader_PARAM.jar
GOTO INICIO

