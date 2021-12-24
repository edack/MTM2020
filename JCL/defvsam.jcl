//DEFVSAM   JOB 1,NOTIFY=&SYSUID
//S1   EXEC PGM=IDCAMS
//SYSPRINT  DD SYSOUT=*
//SYSIN     DD *,SYMBOLS=CNVTSYS
  DELETE &SYSUID..VSAMDS
  SET MAXCC=0
  DEFINE CLUSTER ( NAME ( &SYSUID..VSAMDS) -
            VOLUME(VPWRKD) TRACKS(15) )
//S2   EXEC PGM=IDCAMS
//SYSPRINT  DD SYSOUT=*
//I1        DD DISP=SHR,DSN=ZXP.PUBLIC.SAMPDATA
//O1        DD DISP=SHR,DSN=&SYSUID..VSAMDS
//SYSIN     DD *
  REPRO INFILE(I1) OUTFILE(O1)

  PRINT INFILE(O1) -
         COUNT(20) -
         CHARACTER
/*
