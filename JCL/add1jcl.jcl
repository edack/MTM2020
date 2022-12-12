//ADD1JCL  JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..SOURCE(ADD1CBL),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(ADD1CBL),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=ADD1CBL
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//PRTDONE   DD DSN=Z00070.OUTPUT(PRTDONE),DISP=SHR
//PRTLINE   DD DSN=Z00070.OUTPUT(PRTLINE),DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
