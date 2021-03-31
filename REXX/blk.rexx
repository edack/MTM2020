/***************** REXX *************************/
/* TRACE ?R */
ARG reclen dasd
IF ARG() = 0 THEN DO
   SAY 'Record length missing as first parm.  Exec terminating.'
   EXIT
END
IF dasd = '3380' THEN DO
   trksiz = 47476; halftrk = 23476
END
ELSE DO
   trksiz = 56664; halftrk = 27998; dasd='3390'
END

DO 4; SAY ' '; END
SAY '           ' dasd 'DASD Space Utilization for Record Size' reclen
blkfctr=1;ind=0
blksiz=reclen
SAY 'blk            max        %           blk            max       %'
SAY 'fctr  blksize  blk/TRK   util         fctr  blksize  blk/TRK  util'
DO WHILE blksiz <= halftrk
  IF dasd = '3390' THEN DO
     dn=(blksiz+6)/232
     dni=TRUNC(dn)
     IF dn-dni > 0 THEN dni=dni+1
     sp=19+(blksiz+(6*dni)+6)/34
     spi=TRUNC(sp)
     IF sp-spi > 0 THEN spi=spi+1
     max=TRUNC(1729/spi)
     END
    ELSE DO
     dn=15+(blksiz+12)/32
     dni=TRUNC(dn)
     if dn-dni > 0 THEN dni=dni+1
     max=TRUNC(1499/dni)
     END
  if oldmax = 'OLDMAX' then oldmax=max
  perc=TRUNC((blksiz*max)/trksiz*100,1)
  IF max \= oldmax THEN CALL storeit
  savblkfctr=blkfctr
  savperc=perc
  savblksiz=blksiz
  savmax=max
  blkfctr=blkfctr+1
  blksiz=reclen*blkfctr
END
CALL storeit
CALL storeit
j=TRUNC((ind)/2)
DO i=1 TO j BY 1
  k=i+j
  SAY FORMAT(prblkfctr.i,3) FORMAT(prblksiz.i,9) FORMAT(prmax.i,5),
      FORMAT(prperc.i,6)'%     |' FORMAT(prblkfctr.k,5),
      FORMAT(prblksiz.k,9) FORMAT(prmax.k,5) FORMAT(prperc.k,6)'%'
END
EXIT
storeit:
 ind=ind+1
 oldmax=max
 prblkfctr.ind=savblkfctr
 prperc.ind=savperc
 prblksiz.ind=savblksiz
 prmax.ind=savmax
RETURN
