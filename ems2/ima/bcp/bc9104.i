/***********************************************************************************
** Include...: bc9104.i
** Objetivo..: Chama o update dos campos da interface caracter do Data Collection
** Autor.....: Carlos Alberto Soares Pereira - Maio/2000
** Parametros: 1 - Frame 
**             2 - Indicador de Frame Repetitiva.
***********************************************************************************/

    &IF Defined(Frame01Name) &THEN
        {bcp/bc9114.i &FrameNum=01}        
    &ENDIF
    
    /***********************************************************************************/

    &IF Defined(Frame02Name) &THEN
        {bcp/bc9114.i &FrameNum=02}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame03Name) &THEN
        {bcp/bc9114.i &FrameNum=03}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame04Name) &THEN
        {bcp/bc9114.i &FrameNum=04}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame05Name) &THEN
        {bcp/bc9114.i &FrameNum=05}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame06Name) &THEN
        {bcp/bc9114.i &FrameNum=06}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame07Name) &THEN
        {bcp/bc9114.i &FrameNum=07}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame08Name) &THEN
        {bcp/bc9114.i &FrameNum=08}        
    &ENDIF

    /***********************************************************************************/

    &IF Defined(Frame09Name) &THEN
        {bcp/bc9114.i &FrameNum=09}        
    &ENDIF
    /***********************************************************************************/

    &IF Defined(Frame10Name) &THEN
        {bcp/bc9114.i &FrameNum=10}        
    &ENDIF

    &IF Defined(Frame10Name) &THEN
        End. /* Repeat {&Frame10Name} */
    &ENDIF
    
    &IF Defined(Frame09Name) &THEN
        End. /* Repeat {&Frame09Name} */
    &ENDIF
    
    &IF Defined(Frame08Name) &THEN
        End. /* Repeat {&Frame08Name} */
    &ENDIF
    
    &IF Defined(Frame07Name) &THEN
        End. /* Repeat {&Frame07Name} */
    &ENDIF
    
    &IF Defined(Frame06Name) &THEN
        End. /* Repeat {&Frame06Name} */
    &ENDIF
    
    &IF Defined(Frame05Name) &THEN
        End. /* Repeat {&Frame05Name} */
    &ENDIF
    
    &IF Defined(Frame04Name) &THEN
        End. /* Repeat {&Frame04Name} */
    &ENDIF
    
    &IF Defined(Frame03Name) &THEN
        End. /* Repeat {&Frame03Name} */
    &ENDIF
    
    &IF Defined(Frame02Name) &THEN
        End. /* Repeat {&Frame02Name} */
    &ENDIF
    
    &IF Defined(Frame01Name) &THEN
        End. /* Repeat {&Frame01Name} */
    &ENDIF
    
