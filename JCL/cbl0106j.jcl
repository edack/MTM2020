//CBL0106J JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(CBL0106A),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..COPYLIB,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(CBL0106A),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=CBL0106A
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..DATA.DATA,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
