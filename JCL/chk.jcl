//Z00070A  JOB 1,NOTIFY=&SYSUID
//Z00070A  EXEC PGM=IKJEFT01,REGION=6M
//SYSEXEC  DD DSN=VENDOR.CLIST,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSTERM  DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=CNVTSYS
%CHK &SYSUID PDS1
%CHK &SYSUID PDS2
%CHK &SYSUID USS1
%CHK &SYSUID USS2
%CHK &SYSUID JCL1
%CHK &SYSUID JCL2
%CHK &SYSUID VSC2
%CHK &SYSUID ZOUA1
%CHK &SYSUID ZOUA2
%CHK &SYSUID REXX1
%CHK &SYSUID REXX2
%CHK &SYSUID CBL1
%CHK &SYSUID ZCLI1
%CHK &SYSUID ANSB1
%CHK &SYSUID ANSB2
