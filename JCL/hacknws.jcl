//HACKWS  JOB Z80843,'HACKER NEWS',MSGCLASS=A,CLASS=A,
// MSGLEVEL=(1,1),REGION=0M,SYSAFF=ANY,NOTIFY=&SYSUID
//************************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HACKNWS),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HACKNWS),DISP=SHR
//************************************************************/
// IF RC = 0 THEN
//************************************************************/
//RUN     EXEC PGM=HACKNWS
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//HACKNWS   DD DSN=ZOS.PUBLIC.HACKER.NEWS,DISP=SHR
//*              DCB=(RECFM=FB,LRECL=143)
//SORTFL    DD DSN=&&TEMPFL1,
//             DISP=(NEW,PASS,DELETE),
//             DCB=(LRECL=134,BLKSIZE=27872,RECFM=FB)
//PRTLINE   DD SYSOUT=*,OUTLIM=3400
//SYSOUT    DD SYSOUT=*,OUTLIM=3400
//CEEDUMP   DD DUMMY
//SYSDUMP   DD DUMMY
//************************************************************/
// ELSE
// ENDIF
//SRTSTP  EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSN=&&TEMPFL1,
//             DISP=(OLD,DELETE,DELETE),
//             DCB=(RECFM=FB,LRECL=134,BLKSIZE=27872)
//SORTOUT   DD SYSOUT=*
//RPT1      DD SYSOUT=*
//SYSIN     DD *
  SORT FORMAT=CH,
       FIELDS=(124,10,D)
  OUTFIL FNAMES=RPT1,LINES=50,
  HEADER2=(31:'HACKER NEWS STATUS REPORT FOR ',66:&DATE,
           115:' - PAGE ',&PAGE,2/,
            1:131C'-',/,
            2:' KEY',26:'   TITLE    ',91:' PTS  CMT ',
            102:'AUTHOR',118:'TIME',126:'RATING',/,
            1:131C'-'),
  OUTREC=(1,8,X,
            9,80,X,
            95,4,X,
            99,4,X,
            103,15,X,
            118,5,X,
            126,9)
/*
