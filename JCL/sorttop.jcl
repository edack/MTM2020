//SORTTOPJ  JOB &SYSUID,'TOPACCT SORT',MSGCLASS=A,CLASS=A,
// MSGLEVEL=(1,1),REGION=0M,SYSAFF=ANY
//SRTSTP  EXEC PGM=SORT
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSN=MTM2020.PUBLIC.INPUT(CUSTRECS),
//             DISP=(OLD,KEEP,KEEP),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=32720)
//SORTOUT   DD DUMMY
//RPT1      DD SYSOUT=*
//SYSIN     DD *
  SORT FORMAT=CH,
      FIELDS=(62,12,A)
      INCLUDE   COND=(61,13,CH,GT,C' 8,500,000.00')
  OUTFIL FNAMES=RPT1,BLKCCT1,LINES=50,
  HEADER2=(1:'REPORT OF TOP ACCOUNT BALANCE HOLDERS',/,
           1:'PREPARED FOR ED ACKERMAN/z00070',66:&DATE,/,
           1:'F NAME',13:'L NAME',38:'ACT BALANCE',/,
           1:'======',13:'======',38:'=== =======',/),
  OUTREC=(1,11,X,
            12,22,X,
            61,13,X,
            25X),
  TRAILER1(/,3:COUNT=(EDIT=(IIT)),' Accounts Over Limit')
/*
