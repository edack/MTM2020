/* REXX                                  */
/*                                       */
/* AUTHOR: Mark Zelden                   */
/*                                       */
/* Trace ?r */
/*********************************************************************/
/* DISPLAY SYSTEM INFORMATION ON TERMINAL                            */
/*********************************************************************/
/* IPLINFO can be called as an interactive exec or in batch to       */
/* display various system information.  The result will be displayed */
/* in an ISPF browse data set if ISPF is active.  IPLINFO can also   */
/* be called as a REXX function to return from 1 to 10 variables     */
/* used in the exec at their final value.  If more than one variable */
/* is requested the variables are returned with a space between      */
/* each variable so they can be parsed if desired.  See below for    */
/* the syntax of each method.                                        */
/*********************************************************************/
/* EXECUTION SYNTAX:                                                 */
/*                                                                   */
/* TSO %IPLINFO <option>                                             */
/*                                                                   */
/* VALID OPTIONS ARE 'ALL' (default), 'IPL', 'VERSION', 'STOR',      */
/*                    'CPU', 'IPA', 'SYMBOLS', 'VMAP', 'PAGE',       */
/*                    'SMF', 'SUB' and 'ASID'                        */
/*                                                                   */
/* Examples:                                                         */
/*  TSO %IPLINFO         (Display all information)                   */
/*  TSO %IPLINFO VMAP    (Display a Virtual Storage Map)             */
/*  TSO %IPLINFO SYMBOLS (Display Static System Symbols)             */
/*  TSO %IPLINFO SUB     (Display Subsystem Information)             */
/*                                                                   */
/* Sample Unix System Services WEB Server execution via links:       */
/*  <a href="/cgi-bin/iplinfo">MVS Information</a>                   */
/*  <a href="/cgi-bin/iplinfo?vmap">Virtual Storage Map</a>          */
/*  <a href="/cgi-bin/iplinfo?symbols">Static System Symbols</a>     */
/*  <a href="/cgi-bin/iplinfo?sub">Subsystem Information</a>         */
/*                                                                   */
/*********************************************************************/
/* FUNCTION SYNTAX:                                                  */
/*                                                                   */
/* IPLINFO(VAR,var1_name)                                            */
/* IPLINFO(VAR,var1_name,var2_name,var3_name, ... var10_name)        */
/*                                                                   */
/* Examples:                                                         */
/*  sysname = IPLINFO(VAR,GRSNAME)                                   */
/*  pvtsize = IPLINFO(VAR,GDAPVTSZ)                                  */
/*  ipl_sum = IPLINFO(VAR,IPLDATE,IPLTIME,IPLADDR,IPLVOL,IPLPARM)    */
/*                                                                   */
/*********************************************************************/
LASTUPD = '04/03/2006'                       /* date of last update  */
/*********************************************************************/
VMAP = 'HIGHFIRST'       /* new default - show VMAP from top down    */
/* VMAP = 'LOWFIRST' */  /* the old way - show from bottom up        */
/*  Please let me know if you "need" the old way (LOWFIRST) as I     */
/*  will probably remove the duplicate code in the future.           */
/*********************************************************************/
Arg OPTION,VAR.1,VAR.2,VAR.3,VAR.4,VAR.5,VAR.6,VAR.7,VAR.8,VAR.9,VAR.10
Parse source . EXEC_TYPE . . . . . ENV . .
If ENV <> 'OMVS' then                        /* are we under unix ?  */
  If Sysvar(SYSISPF)='ACTIVE' then do        /* no, is ISPF active?  */
    Address ISREDIT "MACRO (OPTION)"    /* YES - allow use as macro  */
    OPTION = Translate(OPTION)   /* ensure upper case for edit macro */
    Address ISPEXEC "VGET ZENVIR"            /* ispf version         */
    SYSISPF = 'ACTIVE'                       /* set SYSISPF = ACTIVE */
  End
If OPTION <> 'IPL'     & OPTION <> 'VERSION' & OPTION <> 'STOR'    &,
   OPTION <> 'CPU'     & OPTION <> 'IPA'     & OPTION <> 'SYMBOLS' &,
   OPTION <> 'VMAP'    & OPTION <> 'PAGE'    & OPTION <> 'SMF'     &,
   OPTION <> 'SUB'     & OPTION <> 'ASID'    & OPTION <> 'ALL'     &,
   Substr(OPTION,1,3) <> 'VAR' ,
   then OPTION = 'ALL'  /* change to IPL (or other opt) for default  */
                        /* may want to change help panel if changed  */
Numeric digits 10                           /* dflt of 9 not enough  */
Call HEADING                                /* Heading sub-routine   */
Call COMMON            /* control blocks needed by multiple routines */
If OPTION = 'ALL' |  Substr(OPTION,1,3) = 'VAR' then do
  Call IPL                                  /* IPL information       */
  Call VERSION                              /* Version information   */
  Call STOR                                 /* Storage information   */
  Call CPU                                  /* CPU information       */
  Call IPA                                  /* Initialization info.  */
  Call SYMBOLS                              /* Symbols information   */
  Call VMAP                                 /* Virt. Storage Map     */
  Call PAGE                                 /* Page DSN information  */
  Call SMF                                  /* SMF DSN information   */
  Call SUB                                  /* Subsystem information */
  Call ASID                                 /* ASID usage information*/
End
Else interpret call OPTION
/*********************************************************************/
/* Done looking at all control blocks                                */
/*********************************************************************/
/*********************************************************************/
/* IPLINFO called as a function. Return variable names and exit      */
/*********************************************************************/
If Substr(OPTION,1,3) = 'VAR' & EXEC_TYPE='FUNCTION' then do
  "DROPBUF"                                   /* remove data stack   */
  ALL_VARS = Value(VAR.1)                     /* at least one var    */
  Do V = 2 to 10                              /* check for others    */
    If VAR.V = '' then leave                  /* done, leave loop    */
    Else ALL_VARS = ALL_VARS Value(VAR.V)     /* concat var at end   */
  End  /* end Do V */
  Return ALL_VARS                             /* return vars         */
  Exit 0                                      /* End IPLINFO - RC 0  */
End
/*********************************************************************/
/* If ISPF is active, browse output - otherwise write to the terminal*/
/*********************************************************************/
If SYSISPF = 'ACTIVE' then call BROWSE_ISPF  /* ISPF active?         */
Else do queued()                             /* ISPF is not active   */
  Parse pull line                            /* pull queued lines    */
  Say line                                   /* say lines            */
End /* else do  */
Exit 0                                       /* End IPLINFO - RC 0   */
/*********************************************************************/
/*  End of main IPLINFO code                                         */
/*********************************************************************/
/*********************************************************************/
/*  Start of sub-routines                                            */
/*********************************************************************/
HEADING:             /* Heading sub-routine                          */
If ENV = 'OMVS' then do                    /* Are we under OMVS?     */
  Do CKWEB = __ENVIRONMENT.0 to 1 by -1    /* check env. vars        */
     If pos('HTTP_',__ENVIRONMENT.CKWEB) <> 0 then do  /* web server */
       Say 'Content-type: text/html'
       Say ''
       Say '<title>Mark''s MVS Utilities - IPLINFO</title>'
       Say '<meta name="author" content="Mark Zelden -' ,
           'mzelden@flash.net">'
       Say '<meta name="description" content="' || ,
           'IPLINFO -' OPTION 'option.' ,
           'Last updated on' LASTUPD ||'. Written by' ,
           'Mark Zelden. z/OS -- MVS Utilities -' ,
           'http://home.flash.net/~mzelden/mvsutil.html">'
       Say '<meta http-equiv="pragma" content="no-cache">'
       Say '<body BGCOLOR="#000000" TEXT="#00FFFF">'
       Say '<pre>'
       Leave                               /* exit loop              */
     End /* if pos */
  End /* do CKWEB */
End
Call RDATE TODAY                           /* call RDATE sub-routine */
DAY      = Word(RESULT,3)                  /* weekday from RDATE     */
DATE     = Substr(RESULT,1,10)             /* date as MM/DD/YYYY     */
JUL      = Substr(RESULT,7,8)              /* date as YYYY.DDD       */
CURNNNNN = Substr(RESULT,16,5)             /* date as NNNNN          */
Queue '********************************************************' || ,
      '***********************'
Queue '************************  SYSTEM INFORMATION  **********' || ,
      '***********************'
Queue '********************************************************' || ,
      '***********************'
Queue ' '
Queue 'Today is 'DAY DATE '('JUL'). The local time is 'TIME()'.'
return

COMMON:              /* Control blocks needed by multiple routines   */
CVT      = C2d(Storage(10,4))                /* point to CVT         */
PRODNAME = Storage(D2x(CVT - 40),7)          /* point to mvs version */
If Substr(PRODNAME,3,1) > 3 then
  ECVT     = C2d(Storage(D2x(CVT + 140),4))  /* point to CVTECVT     */
FMIDNUM  = Storage(D2x(CVT - 32),7)          /* point to fmid        */
JESCT    = C2d(Storage(D2x(CVT + 296),4))    /* point to JESCT       */
JESPJESN = Storage(D2x(JESCT + 28),4)        /* name of primary JES  */
CSD      = C2d(Storage(D2x(CVT + 660),4))    /* point to CSD         */
SMCA     = Storage(D2x(CVT + 196),4)         /* point to SMCA        */
SMCA     = Bitand(SMCA,'7FFFFFFF'x)          /* zero high order bit  */
SMCA     = C2d(SMCA)                         /* convert to decimal   */
MODEL    = C2d(Storage(D2x(CVT - 6),2))      /* point to cpu model   */
/*********************************************************************/
/*  The CPU model is stored in packed decimal format with no sign,   */
/*  so to make the model printable, it needs to be converted back    */
/*  to hex.                                                          */
/*********************************************************************/
MODEL    = D2x(MODEL)                        /* convert back to hex  */
PCCAVT    = C2d(Storage(D2x(CVT + 764),4))   /* point to PCCA vect tb*/
If Substr(FMIDNUM,4,4) >= 6602 then do
  ECVTIPA  = C2d(Storage(D2x(ECVT + 392),4)) /* point to IPA         */
  IPASCAT  = Storage(D2x(ECVTIPA + 224),63)  /* SYSCAT  card image   */
End
zARCH = 1                                    /* default ARCHLVL      */
If Substr(FMIDNUM,4,4) >  6609 then do       /* OS/390 R10 or above  */
  FLCARCH  = Storage(A3,1)                   /* FLCARCH in PSA       */
  If C2d(FLCARCH) <> 0 then zARCH=2          /* non-zero is z/Arch.  */
End
Return

IPL:                 /* IPL information sub-routine                  */
Queue ' '
/*********************************************************************/
/*  The IPL date is stored in packed decimal format - so to make     */
/*  the date printable, it needs to be converted back to hex and     */
/*  the packed sign needs to be removed.                             */
/*********************************************************************/
IPLTIME  = C2d(Storage(D2x(SMCA + 336),4))   /* IPL Time - binary    */
IPLDATE  = C2d(Storage(D2x(SMCA + 340),4))   /* IPL Date - 0CYYDDDF  */
If IPLDATE  >= 16777231 then do              /*          is C = 1 ?  */
  IPLDATE  = D2x(IPLDATE)                    /* convert back to hex  */
  IPLDATE  = Substr(IPLDATE,2,5)             /* keep YYDDD           */
  IPLDATE  = '20'IPLDATE                     /* use 21st century date*/
End
Else do
  IPLDATE  = D2x(IPLDATE)                    /* convert back to hex  */
  IPLDATE  = Left(IPLDATE,5)                 /* keep YYDDD           */
  IPLDATE  = '19'IPLDATE                     /* use 20th century date*/
End
IPLYYYY  = Substr(IPLDATE,1,4)               /* YYYY portion of date */
IPLDDD   = Substr(IPLDATE,5,3)               /* DDD  portion of date */
Call RDATE IPLYYYY IPLDDD                    /* call RDATE subroutine*/
IPLDAY   = Word(RESULT,3)                    /* weekday from RDATE   */
IPLDATE  = Substr(RESULT,1,10)               /* date as MM/DD/YYYY   */
IPLJUL   = Substr(RESULT,7,8)                /* date as YYYY.DDD     */
IPLNNNNN = Substr(RESULT,16,5)               /* date as NNNNN        */
IPLHH    = Right(IPLTIME%100%3600,2,'0')     /* IPL hour             */
IPLMM    = Right(IPLTIME%100//3600%60,2,'0') /* IPL minute           */
IPLSS    = Right(IPLTIME%100//60,2,'0')      /* IPL seconds          */
IPLTIME  = IPLHH':'IPLMM':'IPLSS             /* time in HH:MM:SS     */
/*                                                                   */
ASMVT    = C2d(Storage(D2x(CVT + 704),4))    /* point to ASMVT       */
CLPABYTE = Storage(D2x(ASMVT + 1),1)         /* point to CLPA byte   */
CHKCLPA  = Bitand(CLPABYTE,'8'x)             /* check for B'1000'    */
CHKCLPA  = C2d(CHKCLPA)                      /* convert to decimal   */
If CHKCLPA < 8 then IPLCLPA = 'with CLPA'    /* bit off - CLPA       */
  Else IPLCLPA = 'without CLPA'              /* bit on  - no CLPA    */
RESUCB   = C2d(Storage(D2x(JESCT + 4),4))    /* point to SYSRES UCB  */
IPLVOL   = Storage(D2x(RESUCB + 28),6)       /* point to IPL volume  */
If Substr(PRODNAME,3,1) < 5 then ,
  IPLADDR  = Storage(D2x(RESUCB + 13),3)     /* point to IPL address */
Else do
  CVTSYSAD = C2d(Storage(D2x(CVT + 48),4))   /* point to UCB address */
  IPLADDR  = Storage(D2x(CVTSYSAD + 4),2)    /* point to IPL UCB     */
  IPLADDR  = C2x(IPLADDR)                    /* convert to EBCDIC    */
End
GRSNAME  = Storage(D2x(CVT + 340),8)         /* point to system name */
GRSNAME  = Strip(GRSNAME,T)                  /* del trailing blanks  */
SMFNAME  = Storage(D2x(SMCA + 16),4)         /* point to SMF name    */
SMFNAME  = Strip(SMFNAME,T)                  /* del trailing blanks  */
If Substr(FMIDNUM,4,4) <  6604 then do       /* use CAXWA B4 OS390R4 */
  AMCBS    = C2d(Storage(D2x(CVT + 256),4))  /* point to AMCBS       */
  ACB      = C2d(Storage(D2x(AMCBS + 8),4))  /* point to ACB         */
  CAXWA    = C2d(Storage(D2x(ACB + 64),4))   /* point to CAXWA       */
  MCATDSN  = Storage(D2x(CAXWA + 52),44)     /* master catalog dsn   */
  MCATDSN  = Strip(MCATDSN,T)                /* remove trailing blnks*/
  MCATUCB  = C2d(Storage(D2x(CAXWA + 28),4)) /* point to mcat UCB    */
  MCATVOL  = Storage(D2x(MCATUCB + 28),6)    /* master catalog VOLSER*/
End
Else do                                      /* OS/390 R4 and above  */
  MCATDSN  = Strip(Substr(IPASCAT,11,44))    /* master catalog dsn   */
  MCATVOL  = Substr(IPASCAT,1,6)             /* master catalog VOLSER*/
End
Queue 'The last IPL was 'IPLDAY IPLDATE '('IPLJUL')' ,
      'at 'IPLTIME' ('CURNNNNN - IPLNNNNN' days ago).'
Queue 'The IPL was done 'IPLCLPA'.'
Queue 'The system IPL address was 'IPLADDR' ('IPLVOL').'
If Substr(PRODNAME,3,1) > 3 then do
  ECVTSPLX = Storage(D2x(ECVT+8),8)          /* point to SYSPLEX name*/
  ECVTLOAD = Storage(D2x(ECVT+160),8)        /* point to LOAD PARM   */
  IPLPARM  = Strip(ECVTLOAD,T)               /* del trailing blanks  */
  SEPPARM  = Substr(IPLPARM,1,4) Substr(IPLPARM,5,2),
             Substr(IPLPARM,7,1) Substr(IPLPARM,8,1)
  SEPPARM  = Strip(SEPPARM,T)                /* del trailing blanks  */
  Queue 'The IPL LOAD PARM used was 'IPLPARM' ('SEPPARM').'
  If Substr(PRODNAME,3,1) >= 5 then do
    CVTIXAVL = C2d(Storage(D2x(CVT+124),4))      /* point to IOCM    */
    IOCIOVTP = C2d(Storage(D2x(CVTIXAVL+208),4)) /* IOS Vector Table */
    CDA      = C2d(Storage(D2x(IOCIOVTP+24),4))  /* point to CDA     */
  End
  If Substr(FMIDNUM,4,4) >= 5520 then do
    ECVTHDNM = Storage(D2x(ECVT+336),8)      /* point to hardware nam*/
    ECVTLPNM = Storage(D2x(ECVT+344),8)      /* point to LPAR name   */
    If Substr(FMIDNUM,4,4) >  6609 then do   /* OS/390 R10 or above  */
      MIFID    = C2d(Storage(D2X(CDA+252),1))  /* MIF ID in decimal  */
      MIFID    = D2x(MIFID)                    /* MIF ID in hex      */
      If zARCH = 2 then ,                    /* z/Architechture      */
        Queue 'The system is running in z/Architecture mode' ,
               '(ARCHLVL = 2).'
      Else ,                                 /* ESA/390 mode         */
        Queue 'The system is running in ESA/390 mode (ARCHLVL = 1).'
    End /* if Substr(FMIDNUM,4,4) >  6609 */
    If ECVTHDNM <> ' ' & ECVTLPNM <> ' ' then do
      CSDPLPN  = C2d(Storage(D2x(CSD + 252),1))    /* point to LPAR #*/
   /* CSDPLPN not valid for z990 (T-REX) or z890 for LPAR number     */
      CPOFF = 0  /* init offset to next PCCA entry                   */
      PCCA  = 0  /* init PCCA to 0                                   */
      Do until PCCA <> 0   /* do until we find a valid PCCA          */
        PCCA = C2d(Storage(D2x(PCCAVT + CPOFF),4)) /* point to PCCA  */
        If PCCA <> 0 then do
          LPAR_#  = X2d(Storage(D2x(PCCA + 7),1))  /*Take first digit*/
        End /* if PCCA <> 0 */
        Else CPOFF = CPOFF + 4  /* bump up offset for next PCCA      */
      End /* do until PCCA <> 0 */
      If Substr(FMIDNUM,4,4) > 6609 then do  /* OS/390 R10 or above  */
        Queue 'The Processor name is' Strip(ECVTHDNM)'.' ,
               'The LPAR name is' Strip(ECVTLPNM)'.'
        Queue ' ' Strip(ECVTLPNM) 'is (HMC defined) LPAR ID =' ,
                   LPAR_# 'and MIF ID =' mifid'.'
        Queue ' ' Strip(ECVTLPNM) 'is PR/SM partition number' ,
                   CSDPLPN' (internal value from the CSD).'
      End /* If Substr(FMIDNUM,4,4) >  6609 */
      Else ,
        Queue 'The Processor name is' Strip(ECVTHDNM)'.' ,
               'The LPAR name is' Strip(ECVTLPNM)' (LPAR #'CSDPLPN').'
    End  /* If ECVTHDNM <> ' ' & ECVTLPNM <> ' '   */
    Else if ECVTHDNM <> ' ' then ,
      Queue 'The Processor name is' Strip(ECVTHDNM)'.'
    If Substr(FMIDNUM,4,4) >= 5510 & ECVTSPLX <> 'LOCAL' then do
      JESCTEXT = C2d(Storage(D2x(JESCT +100),4)) /* point to JESPEXT */
      JESDSNID = X2d(Storage(D2x(JESCTEXT+120),2)) /*ID for temp dsns*/
      Queue 'The sysplex name is' Strip(ECVTSPLX)'. This was system' ,
            'number' Format(JESDSNID) 'added to the sysplex.'
    End /* if Substr(FMIDNUM,4,4) >= 5510*/
    Else queue 'The sysplex name is' Strip(ECVTSPLX)'.'
  End  /* if Substr(FMIDNUM,4,4) >= 5520*/
End
Queue 'The GRS system id (SYSNAME) is 'GRSNAME'.' ,
       'The SMF system id (SID) is 'SMFNAME'.'
If Substr(PRODNAME,3,1) < 5 then do
  IOCON    = Storage(D2x(CVTEXT2 + 6),2)       /* HCD IODFxx or MVSCP*/
                                               /* IOCONFIG ID=xx     */
  Queue 'The currently active IOCONFIG or HCD IODF is 'IOCON'.'
End
Else do
  IODF     = Storage(D2X(CDA+32),44)           /* point to IODF name */
  IODF     = Strip(IODF,T)                     /* del trailing blanks*/
  CONFIGID = Storage(D2X(CDA+92),8)            /* point to CONFIG    */
  EDT      = Storage(D2X(CDA+104),2)           /* point to EDT       */
  IOPROC   = Storage(D2X(CDA+124),8)           /* point to IODF Proc */
  IODATE   = Storage(D2X(CDA+156),8)           /* point to IODF date */
  IOTIME   = Storage(D2X(CDA+164),8)           /* point to IODF time */
  IODESC   = Storage(D2X(CDA+172),16)          /* point to IODF desc */
  Queue 'The currently active IODF data set is 'IODF'.'
  Queue '  Configuration ID =' CONFIGID ' EDT ID =' EDT
  If Substr(IOPROC,1,1) <> '00'x  & ,
     Substr(IOPROC,1,1) <> '40'x then do       /* is token there?    */
    Queue '  TOKEN: Processor  Date      Time      Description'
    Queue '         'IOPROC'   'IODATE'  'IOTIME'  'IODESC
  End
End
Queue 'The Master Catalog is 'MCATDSN' on 'MCATVOL'.'
/*If OPTION = 'IPL' then interpret call 'VERSION' */ /* incl version*/
Return

VERSION:             /* Version information sub-routine              */
Queue ' '
Call SUB FINDJES  /* call SUB routine with FINDJES option            */
If JESPJESN = 'JES3' then do                 /* Is this JES3?        */
  If ENV = 'OMVS' then do  /* running under Unix System Services     */
    JES3FMID = Storage(D2x(JESSSVT+644),8)      /* JES3 FMID         */
    Select  /* determine JES3 version from FMID  */
      When JES3FMID = 'HJS5521' then JESLEV = 'SP 5.2.1'
      When JES3FMID = 'HJS6601' then JESLEV = 'OS 1.1.0'
      When JES3FMID = 'HJS6604' then JESLEV = 'OS 2.4.0'
      When JES3FMID = 'HJS6606' then JESLEV = 'OS 2.6.0'
      When JES3FMID = 'HJS6608' then JESLEV = 'OS 2.8.0'
      When JES3FMID = 'HJS6609' then JESLEV = 'OS 2.9.0'
      When JES3FMID = 'HJS7703' then JESLEV = 'OS 2.10.0'
      When JES3FMID = 'HJS7705' then JESLEV = 'z 1.2.0'
      When JES3FMID = 'HJS7707' then JESLEV = 'z 1.4.0'
      When JES3FMID = 'HJS7708' then JESLEV = 'z 1.5.0'
      When JES3FMID = 'HJS7720' then JESLEV = 'z 1.7.0'
      Otherwise JESLEV = JES3FMID /* if not in tbl, use FMID as ver  */
    End /* select */
    JESNODE  = '*not_avail*'                 /* can't do under USS   */
  End /* if env = 'omvs' */
  Else do /* if not running under Unix System Services, use TSO VARs */
    JESLEV   = SYSVAR(SYSJES)                /* TSO/E VAR for JESLVL */
    JESNODE  = SYSVAR(SYSNODE)               /* TSO/E VAR for JESNODE*/
  End
End
Else do  /* JES2 */
  JESLEV   = Strip(Storage(D2x(JESSUSE),8))  /* JES2 Version         */
  /* offset in $HCCT - CCTNDENM */
  Select
    When Substr(JESLEV,1,8) == 'z/OS 1.7' then, /* z/OS 1.7          */
      JESNODE  = Strip(Storage(D2x(JESSUS2+616),8)) /* JES2 NODE     */
    When Substr(JESLEV,1,8) == 'z/OS 1.5' | , /* z/OS 1.5 & 1.6      */
      Substr(JESLEV,1,8) == 'z/OS 1.4' then   /* z/OS 1.4            */
      JESNODE  = Strip(Storage(D2x(JESSUS2+532),8)) /* JES2 NODE     */
    When Substr(JESLEV,1,7) == 'OS 2.10' | ,  /* OS/390 2.10 and     */
      Substr(JESLEV,1,8) == 'z/OS 1.2' then,  /* z/OS 1.2            */
      JESNODE  = Strip(Storage(D2x(JESSUS2+452),8)) /* JES2 NODE     */
    When Substr(JESLEV,1,6) == 'OS 1.1' | , /* OS/390 1.1  or        */
      Substr(JESLEV,1,4) == 'SP 5' then ,    /* ESA V5 JES2          */
      JESNODE  = Strip(Storage(D2x(JESSUS2+336),8)) /*   JES2 NODE   */
    When Substr(JESLEV,1,5) == 'OS 1.' | ,   /* OS/390 1.2           */
      Substr(JESLEV,1,5) == 'OS 2.' then,    /*  through OS/390 2.9  */
      JESNODE  = Strip(Storage(D2x(JESSUS2+372),8)) /* JES2 NODE     */
    Otherwise ,                              /* Lower than ESA V5    */
      If ENV = 'OMVS' then JESNODE = '*not_avail*'
      else JESNODE  = SYSVAR(SYSNODE)        /* TSO/E VAR for JESNODE*/
  End  /* select */
End /* else do */
/*                                                                   */
CVTVERID = Storage(D2x(CVT - 24),16)         /* "user" software vers.*/
CVTRAC   = C2d(Storage(D2x(CVT + 992),4))    /* point to RACF CVT    */
RCVTID   = Storage(D2x(CVTRAC),4)            /* point to RCVTID      */
                                             /* RCVT, ACF2, or RTSS  */
SECNAM = RCVTID                              /* ACF2 SECNAME = RCVTID*/
If RCVTID = 'RCVT' then SECNAM = 'RACF'      /* RCVT is RACF         */
If RCVTID = 'RTSS' then SECNAM = 'Top Secret'  /* RTSS is Top Secret */
RACFVRM  = Storage(D2x(CVTRAC + 616),4)      /* RACF Ver/Rel/Mod     */
RACFVER  = Substr(RACFVRM,1,1)               /* RACF Version         */
RACFREL  = Substr(RACFVRM,2,2)               /* RACF Release         */
RACFREL  = Format(RACFREL)                   /* Remove leading 0     */
RACFMOD  = Substr(RACFVRM,4,1)               /* RACF MOD level       */
RACFLEV  = RACFVER || '.' || RACFREL || '.' || RACFMOD
If RCVTID = 'RCVT' | RCVTID = 'RTSS' then ,
 RCVTDSN = Strip(Storage(D2x(CVTRAC + 56),44))  /* RACF prim dsn or  */
                                                /* TSS Security File */
/*                                                                   */
CVTDFA   = C2d(Storage(D2x(CVT + 1216),4))   /* point to DFP ID table*/
DFAPROD  = C2d(Storage(D2x(CVTDFA +16),1))   /* point to product byte*/
If DFAPROD = 0 then do                       /* DFP not DF/SMS       */
  DFAREL   = C2x(Storage(D2x(CVTDFA+2),2))   /* point to DFP release */
  DFPVER   = Substr(DFAREL,1,1)              /* DFP Version          */
  DFPREL   = Substr(DFAREL,2,1)              /* DFP Release          */
  DFPMOD   = Substr(DFAREL,3,1)              /* DFP Mod Lvl          */
  DFPRD    = 'DFP'                           /* product is DFP       */
  DFLEV    = DFPVER || '.' || DFPREL || '.' || DFPMOD
End
Else do                                      /* DFSMS not DFP        */
  DFARELS  = C2x(Storage(D2x(CVTDFA+16),4))  /* point to DF/SMS rel  */
  DFAVER   = X2d(Substr(DFARELS,3,2))        /* DF/SMS Version       */
  DFAREL   = X2d(Substr(DFARELS,5,2))        /* DF/SMS Release       */
  DFAMOD   = X2d(Substr(DFARELS,7,2))        /* DF/SMS Mod Lvl       */
  DFPRD    = 'DFSMS'                         /* product is DF/SMS    */
  DFLEV    = DFAVER || '.' || DFAREL || '.' || DFAMOD
  If DFAPROD = 2 then DFLEV = 'OS/390' DFLEV
  If DFAPROD = 3 then DFLEV = 'z/OS' DFLEV
End
/*                                                                   */
CVTTVT   = C2d(Storage(D2x(CVT + 156),4))    /* point to TSO vect tbl*/
TSVTLVER = Storage(D2x(CVTTVT+100),1)        /* point to TSO Version */
TSVTLREL = Storage(D2x(CVTTVT+101),2)        /* point to TSO Release */
TSVTLREL = Format(TSVTLREL)                  /* Remove leading 0     */
TSVTLMOD = Storage(D2x(CVTTVT+103),1)        /* point to TSO Mod Lvl */
TSOLEV   = TSVTLVER || '.' || TSVTLREL || '.' || TSVTLMOD
/*                                                                   */
CVTEXT2  = C2d(Storage(D2x(CVT + 328),4))    /* point to CVTEXT2     */
CHKVTACT = Storage(D2x(CVTEXT2+64),1)        /* VTAM active flag     */
If bitand(CHKVTACT,'80'x) = '80'x then do      /* vtam is active     */
  CVTATCVT = C2d(Storage(D2x(CVTEXT2 + 65),3)) /* point to VTAM AVT  */
  ISTATCVT = C2d(Storage(D2x(CVTATCVT + 0),4)) /* point to VTAM CVT  */
  ATCVTLVL = Storage(D2x(ISTATCVT + 0),8)      /* VTAM Rel Lvl VOVRP */
  VTAMVER  = Substr(ATCVTLVL,3,1)              /* VTAM Version   V   */
  VTAMREL  = Substr(ATCVTLVL,4,1)              /* VTAM Release    R  */
  VTAMMOD  = Substr(ATCVTLVL,5,1)              /* VTAM Mod Lvl     P */
  If VTAMMOD = ' ' then VTAMLEV =  VTAMVER || '.' || VTAMREL
    else VTAMLEV =  VTAMVER || '.' || VTAMREL || '.' || VTAMMOD
/*                                                                   */
  ATCNETID = Strip(Storage(D2x(ISTATCVT + 2080),8))  /* VTAM NETID   */
  ATCNQNAM = Strip(Storage(D2x(ISTATCVT + 2412),17)) /* VTAM SSCPNAME*/
  VTAM_ACTIVE = 'YES'
End /* if bitand (vtam is active) */
Else VTAM_ACTIVE = 'NO'
If Substr(PRODNAME,3,1) < 6 then
  Queue 'The MVS version is 'PRODNAME' - FMID 'FMIDNUM'.'
Else do
  PRODNAM2 = Storage(D2x(ECVT+496),16)       /* point to product name*/
  PRODNAM2 = Strip(PRODNAM2,T)               /* del trailing blanks  */
  VER      = Storage(D2x(ECVT+512),2)        /* point to version     */
  REL      = Storage(D2x(ECVT+514),2)        /* point to release     */
  MOD      = Storage(D2x(ECVT+516),2)        /* point to mod level   */
  VRM      = VER'.'REL'.'MOD
  Queue 'The OS version is 'PRODNAM2 VRM' - FMID' ,
         FMIDNUM' ('PRODNAME').'
End
If CVTVERID <> ' ' then ,
  Queue 'The "user" system software version is' Strip(CVTVERID,T)'.'
Queue 'The primary job entry subsystem is 'JESPJESN'.'
Queue 'The 'JESPJESN 'level is 'JESLEV'.' ,
      'The 'JESPJESN 'node name is 'JESNODE'.'
If SECNAM <> 'RACF' | RACFVRM < '2608' then do
  Queue 'The security software is 'SECNAM'.' ,
        'The RACF level is 'RACFLEV'.'
  If SECNAM = 'Top Secret' then ,
   Queue '  The TSS Security File DSN is' RCVTDSN'.'
  If SECNAM = 'RACF' then ,
   Queue '  The RACF Primary DSN is' RCVTDSN'.'
End
Else do
  Queue 'The security software is' Word(PRODNAM2,1) ,
        'Security Server (RACF).' ,
        'The FMID is HRF' || RACFVRM || '.'
  Queue '  The RACF Primary DSN is' RCVTDSN'.'
End
Queue 'The' DFPRD 'level is' DFLEV'.'
Queue 'The TSO level is 'TSOLEV'.'
If SYSISPF = 'ACTIVE' then do                /* is ISPF active?      */
  Address ISPEXEC "VGET ZISPFOS"             /* yes, is it OS?390?   */
  If RC = 0 then do                          /* yes, get OS/390 var  */
    ISPFLEV = Strip(Substr(ZISPFOS,10,15))   /* only need version    */
    Address ISPEXEC "VGET ZENVIR"            /* ispf internal rel var*/
    ISPFLEVI = Substr(ZENVIR,1,8)            /* internal ISPF release*/
    Queue 'The ISPF level is 'ISPFLEV' ('ISPFLEVI').'
  End  /* if RC */
  Else do                          /* not OS/390 - use old variables */
    Address ISPEXEC "VGET ZPDFREL"           /* get pdf release info */
    ISPFLEV  = Substr(ZENVIR,6,3)            /* ISPF level           */
    PDFLEV   = Substr(ZPDFREL,5,3)           /* PDF  level           */
    Queue 'The ISPF level is 'ISPFLEV'. The PDF level is' PDFLEV'.'
  End /* else do */
End  /* if SYSISPF */
If VTAM_ACTIVE = 'YES' then do
  Queue 'The VTAM level is 'VTAMLEV'.'
  Queue '  The NETID is' ATCNETID'. The SSCPNAME is' ATCNQNAM'.'
End /* if VTAM_ACTIVE = YES */
Else Queue 'The VTAM level is not available - VTAM is not active.'
Return

STOR:                /* Storage information sub-routine              */
Queue ' '
CVTRLSTG = C2d(Storage(D2x(CVT + 856),4))    /* point to store at IPL*/
CVTRLSTG = CVTRLSTG/1024                     /* convert to Megabytes */
If zARCH <> 2 then do                        /* not valid in 64-bit  */
  CVTEORM  = C2d(Storage(D2x(CVT + 312),4))  /* potential real high  */
  CVTEORM  = (CVTEORM+1)/1024/1024           /* convert to Megabytes */
  RCE      = C2d(Storage(D2x(CVT + 1168),4)) /* point to RCE         */
  ESTOR    = C2d(Storage(D2x(RCE + 160),4))  /* point to ESTOR frames*/
  ESTOR    = ESTOR*4/1024                    /* convert to Megabytes */
End
Call STORAGE_GDA_LDA
Queue 'The real storage size at IPL time was 'Format(CVTRLSTG,,0)'M.'
If zARCH <> 2 then do       /* not valid in 64-bit */
  Queue 'The potential real storage size is' ,
         Format(CVTEORM,,0)'M.'
  If ESTOR > 0 then
    Queue 'The expanded storage size is 'ESTOR'M.'
  Else
    Queue 'The system has no expanded storage.'
End
Queue 'The private area size <16M is 'GDAPVTSZ'K.'
Queue 'The private area size >16M is 'GDAEPVTS'M.'
Queue 'The CSA size <16M is 'GDACSASZ'K.'
Queue 'The CSA size >16M is 'GDAECSAS'K.'
Queue 'The SQA size <16M is 'GDASQASZ'K.'
Queue 'The SQA size >16M is 'GDAESQAS'K.'
Queue 'The maximum V=R region size is 'GDAVRSZ'K.'
Queue 'The default V=R region size is 'GDAVREGS'K.'
Queue 'The maximum V=V region size is 'LDASIZEA'K.'
Return

CPU:                 /* CPU information sub-routine                  */
Queue ' '
If Substr(FMIDNUM,4,4) >= 7709 then ,    /* >16 CPs-z/OS 1.6 & above */
  NUMCPU   = C2d(Storage(D2x(CSD + 212),4))  /* point to # of CPUS   */
Else,
  NUMCPU   = C2d(Storage(D2x(CSD + 10),2))   /* point to # of CPUS   */
/*                                                                   */
Queue 'The CPU model number is 'MODEL'.'
Queue 'The number of online CPUs is 'NUMCPU'.'
/*                                                                   */
CPNUM     = 0
FOUNDCPUS = 0
FOUNDZAPS = 0
Do until FOUNDCPUS = NUMCPU
PCCA = C2d(Storage(D2x(PCCAVT + CPNUM*4),4)) /* point to PCCA        */
  If PCCA <> 0 then do
    CPUVER   = Storage(D2x(PCCA + 4),2)      /* point to VERSION     */
    CPUID    = Storage(D2x(PCCA + 6),10)     /* point to CPUID       */
    IDSHORT  = Substr(CPUID,2,5)
    PCCAATTR = Storage(D2x(PCCA + 376),1)    /* attribute byte       */
    PCCARCFF = Storage(D2x(PCCA + 379),1)    /* reconfig flag        */
    CP_TYP   = ''                            /* init to null for now */
    If bitand(PCCAATTR,'01'x) = '01'x then do  /* check PCCAIFA      */
       CP_TYP = '(zAAP)'                       /* zAAP / IFA CP      */
       FOUNDZAPS = FOUNDZAPS + 1
    End
    If bitand(PCCARCFF,'80'x) = '80'x then ,   /* check PCCACWLM     */
       CP_TYP = '(WLM)'                        /* WLM controlled CP  */
    CPNUM_M = D2x(CPNUM)                       /* display in hex     */
    If Substr(FMIDNUM,4,4) >= 7709 then ,      /* z/OS 1.6 and above */
      CPNUM_M = Right(CPNUM_M,2,'0')           /* display as 2 digits*/
    Queue 'The CPU serial number for CPU 'CPNUM_M' is ' || ,
     CPUID' ('IDSHORT'), version code' CPUVER'.' CP_TYP
    FOUNDCPUS = FOUNDCPUS + 1
  End
CPNUM = CPNUM + 1
End  /* do until  */
/**************************************************/
/* SUs/SEC and MIPS calculations                  */
/* SYS1.NUCLEUS(IEAVNP10) CSECT IRARMCPU          */
/**************************************************/
RMCT     = C2d(Storage(D2x(CVT+604),4))      /* point to RMCT        */
SU       = C2d(Storage(D2x(RMCT+64),4))      /* CPU Rate Adjustment  */
SUSEC    = Format((16000000/SU),7,2)         /* SUs per second       */
MIPSCP   = NUMCPU-FOUNDZAPS                  /* Don't include zAAPs  */
MIPS     = Format((SUSEC/48.5) * MIPSCP,6,2) /* SRM MIPS calculation */
Queue 'The service units per second per online CPU is' Strip(SUSEC)'.'
Queue 'The approximate total MIPS (SUs/SEC / 48.5 * # non-zAAP CPUs)' ,
      'is' Strip(MIPS)'.'
/**************************************************/
/* Central Processing Complex Node Descriptor     */
/**************************************************/
If Substr(PRODNAME,3,1) >= 5 then do
  CVTHID   = C2d(Storage(D2x(CVT + 1068),4))   /* point to SHID      */
  CPCND_FLAGS = Storage(D2x(CVTHID+22),1)      /* pnt to CPCND FLAGS */
  If CPCND_FLAGS <> 0 then do                  /* Is there a CPC?    */
    CPCND_VALID = Bitand(CPCND_FLAGS,'E0'x)    /* Valid flags        */
    CPCND_INVALID = Bitand('40'x)              /* Invalid flag       */
    If CPCND_VALID <> CPCND_INVALID then do    /* Is it valid?       */
      CPCND_TYPE  = Storage(D2x(CVTHID+26),6)  /* Type               */
      CPCND_MODEL = Storage(D2x(CVTHID+32),3)  /* Model              */
      CPCND_MAN   = Storage(D2x(CVTHID+35),3)  /* Manufacturer       */
      CPCND_PLANT = Storage(D2x(CVTHID+38),2)  /* Plant of manufact. */
      CPCND_SEQNO = Storage(D2x(CVTHID+40),12) /* Sequence number    */
      CPC_ID      = C2x(Storage(D2x(CVTHID+55),1))  /* CPC ID        */
      Queue ' '
      Queue 'Central Processing Complex (CPC) Node Descriptor:'
      Queue '  CPC ND =',
       CPCND_TYPE'.'CPCND_MODEL'.'CPCND_MAN'.'CPCND_PLANT'.'CPCND_SEQNO
      Queue '  CPC ID =' CPC_ID
      Queue '  Type('CPCND_TYPE') Model('CPCND_MODEL')',
            'Manufacturer('CPCND_MAN') Plant('CPCND_PLANT')',
            'Seq Num('CPCND_SEQNO')'
    End /* if CPCND_VALID <> CPCND_INVALID */
  End  /* if CPCND_FLAGS <>0  */
End
Return

IPA:                 /* IPA information sub-routine                  */
Queue ' '
/*********************************************************************/
/* IPL parms from the IPA                                            */
/*********************************************************************/
If Substr(FMIDNUM,4,4) >= 6602 then do
  IPALPARM = Storage(D2x(ECVTIPA + 16),8)    /* point to LOAD PARM   */
  IPALPDSN = Storage(D2x(ECVTIPA + 48),44)   /* load parm dsn name   */
  IPAHWNAM = Storage(D2x(ECVTIPA + 24),8)    /* point to HWNAME      */
  IPAHWNAM = Strip(IPAHWNAM,T)               /* del trailing blanks  */
  IPALPNAM = Storage(D2x(ECVTIPA + 32),8)    /* point to LPARNAME    */
  IPALPNAM = Strip(IPALPNAM,T)               /* del trailing blanks  */
  IPAVMNAM = Storage(D2x(ECVTIPA + 40),8)    /* point to VMUSERID    */
  /**************************/
  /* PARMS in LOADxx        */
  /**************************/
  IPANUCID = Storage(D2x(ECVTIPA + 23),1)    /* NUCLEUS ID           */
  IPAIODF  = Storage(D2x(ECVTIPA + 96),63)   /* IODF    card image   */
  IPASPARM = Storage(D2x(ECVTIPA + 160),63)  /* SYSPARM card image   */
  /*IPASCAT= Storage(D2x(ECVTIPA + 224),63)*//* SYSCAT  card image   */
  IPASYM   = Storage(D2x(ECVTIPA + 288),63)  /* IEASYM  card image   */
  IPAPLEX  = Storage(D2x(ECVTIPA + 352),63)  /* SYSPLEX card image   */
  IPAPLNUM = Storage(D2x(ECVTIPA + 2148),2)  /* number of parmlibs   */
  IPAPLNUM = C2d(IPAPLNUM)                   /* convert to decimal   */
  POFF = 0
  Do P = 1 to IPAPLNUM
    IPAPLIB.P = Storage(D2x(ECVTIPA+416+POFF),63) /* PARMLIB cards   */
    IPAPLFLG.P = Storage(D2x(ECVTIPA+479+POFF),1)  /* flag bits      */
    If bitand(IPAPLFLG.P,'20'x) = '20'x then ,   /* volser from cat? */
      IPAPLIB.P = Overlay('      ',IPAPLIB.P,46) /* no, clear it     */
    POFF = POFF + 64
  End
  IPANLID  = Storage(D2x(ECVTIPA + 2144),2)  /* NUCLSTxx member used */
  IPANUCW  = Storage(D2x(ECVTIPA + 2146),1)  /* load wait state char */
  Queue 'Initialization information from the IPA:'
  Queue '  IPLPARM =' IPALPARM   '(merged)'
  Queue '  IPL load parameter data set name: 'IPALPDSN
  Queue '  HWNAME='IPAHWNAM '  LPARNAME='IPALPNAM ,
        '  VMUSERID='IPAVMNAM
  Queue '  '                    /* add blank line for readability   */
  Queue '  LOADxx parameters from the IPA' ,
        '(LOAD' || Substr(IPALPARM,5,2) || '):'
  Queue '    *---+----1----+----2----+----3----+----4' || ,
            '----+----5----+----6----+----7'
  If Substr(FMIDNUM,4,4) >  6609 then do     /* OS/390 R10 or above  */
    IPAARCHL = Storage(D2x(ECVTIPA + 2143),1)  /* ARCHLVL (1 or 2)   */
    Queue '    ARCHLVL  'IPAARCHL
  End
  If IPASYM   <> '' then queue '    IEASYM   'IPASYM
  If IPAIODF  <> '' then queue '    IODF     'IPAIODF
  If IPANUCID <> '' then queue '    NUCLEUS  'IPANUCID
  If IPANLID  <> '' then queue '    NUCLST   'IPANLID' 'IPANUCW
  Do P = 1 to IPAPLNUM
    Queue '    PARMLIB  'IPAPLIB.P
  End
  If IPASCAT  <> '' then queue '    SYSCAT   'IPASCAT
  If IPASPARM <> '' then queue '    SYSPARM  'IPASPARM
  If IPAPLEX  <> '' then queue '    SYSPLEX  'IPAPLEX
  /**************************/
  /* PARMS in IEASYSxx      */
  /**************************/
  Queue '  '                    /* add blank line for readability   */
  Queue '  IEASYSxx parameters from the IPA:          ',
        '                     (Source)'
  Call BUILD_IPAPDETB    /* Build table for init parms               */
  TOTPRMS = 0            /* tot num of specified or defaulted parms  */
  Do I = 1 to IPAPDETB.0
    Call EXTRACT_SYSPARMS IPAPDETB.I   /* extract parms from the IPA */
  End
 /********************************************************************/
 /* Uncommment a sample below to test IPA PAGE parm "split" code:    */
 /*  PRMLINE.32 = 'SWAP SWAP=(SYS1.SWAP.TEST) IEASYSXX'              */
 /*  PRMLINE.32 = 'NONVIO NONVIO=(SYS1.PAGE.TEST) IEASYSXX'          */
 /*  PRMLINE.32 = 'NONVIO NONVIO=(SYS1.PAGE1,SYS1.PAGE2) IEASYSXX'   */
 /*  PRMLINE.32 = 'NONVIO ' || ,                                     */
 /*  'NONVIO=(SYS1.PAGE1,SYS1.PAGE2,SYS1.PAGE3,SYS1.PAGE4) IEASYSXX' */
 /********************************************************************/
  Call SORT_IPA                       /* sort IPA parms              */
  Call SPLIT_IPA_PAGE                 /* split page/swap dsn parms   */
  Do I = 1 to TOT_IPALINES            /* add ipa parms               */
    If I = TOT_IPALINES then ,        /*   to stack and              */
      IPALINE.I = Translate(IPALINE.I,' ',',') /* remove comma       */
    Queue IPALINE.I                   /*           from last parm    */
  End
End
Return

SYMBOLS:             /* System Symbols information sub-routine       */
Queue ' '
/*********************************************************************/
/* Find System Symbols  - ASASYMBP MACRO                             */
/*  ECVT+X'128' = ECVTSYMT                                           */
/*  2nd half word = # of symbols , after that each entry is 4 words  */
/*  1st word = offset to symbol name                                 */
/*  2nd word = length of symbol name                                 */
/*  3rd word = offset to symbol value                                */
/*  4th word = length of symbol value                                */
/*********************************************************************/
If Substr(FMIDNUM,4,4) >= 5520 then do
  ECVTSYMT = C2d(Storage(D2x(ECVT + 296),4)) /* point to ECVTSYMT    */
  NUMSYMBS = C2d(Storage(D2x(ECVTSYMT + 2),2))  /* number of symbols */
  Queue 'Static System Symbol Values:'
  Do I = 1 to NUMSYMBS
    SOFF = I*16-16
    NAMOFF  = C2d(Storage(D2x(ECVTSYMT+4+SOFF),4))  /*offset to name */
    NAMLEN  = C2d(Storage(D2x(ECVTSYMT+8+SOFF),4))  /*length of name */
    VALOFF  = C2d(Storage(D2x(ECVTSYMT+12+SOFF),4)) /*offset to value*/
    VALLEN  = C2d(Storage(D2x(ECVTSYMT+16+SOFF),4)) /*length of value*/
    SYMNAME = Storage(D2x(ECVTSYMT+4+NAMOFF),NAMLEN) /*symbol name   */
    If VALLEN = 0 then VALNAME = ''                 /* null value    */
    Else ,
    VALNAME = Storage(D2x(ECVTSYMT+4+VALOFF),VALLEN) /* symbol value */
    Queue ' ' Left(SYMNAME,10,' ') '=' VALNAME
  End  /* do NUMSYMBS */
End
Return

VMAP:                /* Virtual Storage Map sub-routine              */
Queue ' '
If option <> 'ALL' then,
  Call STORAGE_GDA_LDA                       /* GDA/LDA stor routine */
SYSEND  = X2d(LDASTRTS) + (LDASIZS*1024) - 1 /* end of system area   */
SYSEND  = D2x(SYSEND)                        /* display in hex       */
If GDAVRSZ = 0 then do                       /* no v=r               */
  VRSTRT = 'N/A     '
  VREND  = 'N/A     '
  VVSTRT = LDASTRTA                          /* start of v=v         */
  VVEND  =  X2d(LDASTRTA) + (LDASIZEA*1024) - 1 /* end of v=v        */
  VVEND  =  D2x(VVEND)                       /* display in hex       */
End
Else do
  VRSTRT =  LDASTRTA                         /* start of v=r         */
  VREND  =  X2d(LDASTRTA) + (GDAVRSZ*1024) - 1 /* end of v=r         */
  VREND  =  D2X(VREND)                       /* display in hex       */
  VVSTRT =  LDASTRTA                         /* start of v=v         */
  VVEND  =  X2d(LDASTRTA) + (LDASIZEA*1024) - 1 /* end of v=v        */
  VVEND  =  D2x(VVEND)                       /* display in hex       */
End
GDACSA   = C2d(Storage(D2x(CVTGDA + 108),4)) /* start of CSA addr    */
GDACSAH  = D2x(GDACSA)                       /* display in hex       */
CSAEND   = (GDACSASZ*1024) + GDACSA - 1      /* end of CSA           */
CSAEND   = D2x(CSAEND)                       /* display in hex       */
CVTSMEXT = C2d(Storage(D2x(CVT +1196),4))    /* point to stg map ext.*/
CVTMLPAS = C2d(Storage(D2x(CVTSMEXT+ 8),4))  /* start of MLPA addr   */
CVTMLPAS = D2x(CVTMLPAS)                     /* display in hex       */
If CVTMLPAS <> 0 then do
  CVTMLPAE = C2d(Storage(D2x(CVTSMEXT+12),4))  /* end of MLPA addr   */
  CVTMLPAE = D2x(CVTMLPAE)                     /* display in hex     */
  MLPASZ   = X2d(CVTMLPAE) - X2d(CVTMLPAS) + 1 /* size of MLPA       */
  MLPASZ   = MLPASZ/1024                       /* convert to Kbytes  */
End
Else do /* no MLPA */
  CVTMLPAS = 'N/A     '
  CVTMLPAE = 'N/A     '
  MLPASZ   = 0
End
CVTFLPAS = C2d(Storage(D2x(CVTSMEXT+16),4))  /* start of FLPA addr   */
CVTFLPAS = D2x(CVTFLPAS)                     /* display in hex       */
If CVTFLPAS <> 0 then do
  CVTFLPAE = C2d(Storage(D2x(CVTSMEXT+20),4))  /* end of FLPA addr   */
  CVTFLPAE = D2x(CVTFLPAE)                     /* display in hex     */
  FLPASZ   = X2d(CVTFLPAE) - X2d(CVTFLPAS) + 1 /* size of FLPA       */
  FLPASZ   = FLPASZ/1024                       /* convert to Kbytes  */
End
Else do /* no FLPA */
  CVTFLPAS = 'N/A     '
  CVTFLPAE = 'N/A     '
  FLPASZ   = 0
End
CVTPLPAS = C2d(Storage(D2x(CVTSMEXT+24),4))  /* start of PLPA addr   */
CVTPLPAS = D2x(CVTPLPAS)                     /* display in hex       */
CVTPLPAE = C2d(Storage(D2x(CVTSMEXT+28),4))  /* end of PLPA addr     */
CVTPLPAE = D2x(CVTPLPAE)                     /* display in hex       */
PLPASZ   = X2d(CVTPLPAE) - X2d(CVTPLPAS) + 1 /* size of PLPA         */
PLPASZ   = PLPASZ/1024                       /* convert to Kbytes    */
GDASQA   = C2d(Storage(D2x(CVTGDA + 144),4)) /* start of SQA addr    */
GDASQAH  = D2x(GDASQA)                       /* display in hex       */
SQAEND   = (GDASQASZ*1024) + GDASQA - 1      /* end of SQA           */
SQAEND   = D2x(SQAEND)                       /* display in hex       */
CVTRWNS  = C2d(Storage(D2x(CVTSMEXT+32),4))  /* start of R/W nucleus */
CVTRWNS  = D2x(CVTRWNS)                      /* display in hex       */
CVTRWNE  = C2d(Storage(D2x(CVTSMEXT+36),4))  /* end of R/W nucleus   */
CVTRWNE  = D2x(CVTRWNE)                      /* display in hex       */
RWNUCSZ  = X2d(CVTRWNE)  - X2d(CVTRWNS)  + 1 /* size of R/W nucleus  */
RWNUCSZ  = Format(RWNUCSZ/1024,,0)           /* convert to Kbytes    */
CVTRONS  = C2d(Storage(D2x(CVTSMEXT+40),4))  /* start of R/O nucleus */
CVTRONS  = D2x(CVTRONS)                      /* display in hex       */
CVTRONE  = C2d(Storage(D2x(CVTSMEXT+44),4))  /* end of R/O nucleus   */
CVTRONE  = D2x(CVTRONE)                      /* display in hex       */
RONUCSZ  = X2d(CVTRONE)  - X2d(CVTRONS)  + 1 /* size of R/O nucleus  */
RONUCSZ  = Format(RONUCSZ/1024,,0)           /* convert to Kbytes    */
RONUCSZB = X2d('FFFFFF') - X2d(CVTRONS) + 1  /* size of R/O nuc <16M */
RONUCSZB = Format(RONUCSZB/1024,,0)          /* convert to Kbytes    */
RONUCSZA = X2d(CVTRONE) - X2d('1000000') + 1 /* size of R/O nuc >16M */
RONUCSZA = Format(RONUCSZA/1024,,0)          /* convert to Kbytes    */
CVTERWNS = C2d(Storage(D2x(CVTSMEXT+48),4))  /* start of E-R/W nuc   */
CVTERWNS = D2x(CVTERWNS)                     /* display in hex       */
CVTERWNE = C2d(Storage(D2x(CVTSMEXT+52),4))  /* end of E-R/W nuc     */
CVTERWNE = D2x(CVTERWNE)                     /* display in hex       */
ERWNUCSZ = X2d(CVTERWNE) - X2d(CVTERWNS) + 1 /* size of E-R/W nuc    */
ERWNUCSZ = ERWNUCSZ/1024                     /* convert to Kbytes    */
GDAESQA  = C2d(Storage(D2x(CVTGDA + 152),4)) /* start of ESQA addr   */
GDAESQAH = D2x(GDAESQA)                      /* display in hex       */
ESQAEND  = (GDAESQAS*1024) + GDAESQA - 1     /* end of ESQA          */
ESQAEND  = D2x(ESQAEND)                      /* display in hex       */
CVTEPLPS = C2d(Storage(D2x(CVTSMEXT+56),4))  /* start of EPLPA addr  */
CVTEPLPS = D2x(CVTEPLPS)                     /* display in hex       */
CVTEPLPE = C2d(Storage(D2x(CVTSMEXT+60),4))  /* end of EPLPA addr    */
CVTEPLPE = D2x(CVTEPLPE)                     /* display in hex       */
EPLPASZ  = X2d(CVTEPLPE) - X2d(CVTEPLPS) + 1 /* size of EPLPA        */
EPLPASZ  = EPLPASZ/1024                      /* convert to Kbytes    */
CVTEFLPS = C2d(Storage(D2x(CVTSMEXT+64),4))  /* start of EFLPA addr  */
CVTEFLPS = D2x(CVTEFLPS)                     /* display in hex       */
If CVTEFLPS <> 0 then do
  CVTEFLPE = C2d(Storage(D2x(CVTSMEXT+68),4))  /* end of EFLPA addr  */
  CVTEFLPE = D2x(CVTEFLPE)                     /* display in hex     */
  EFLPASZ  = X2d(CVTEFLPE) - X2d(CVTEFLPS) + 1 /* size of EFLPA      */
  EFLPASZ  = EFLPASZ/1024                      /* convert to Kbytes  */
End
Else do /* no EFLPA */
  CVTEFLPS = 'N/A     '
  CVTEFLPE = 'N/A     '
  EFLPASZ  = 0
End
CVTEMLPS = C2d(Storage(D2x(CVTSMEXT+72),4))  /* start of EMLPA addr  */
CVTEMLPS = D2x(CVTEMLPS)                     /* display in hex       */
If CVTEMLPS <> 0 then do
  CVTEMLPE = C2d(Storage(D2x(CVTSMEXT+76),4))  /* end of EMLPA addr  */
  CVTEMLPE = D2x(CVTEMLPE)                     /* display in hex     */
  EMLPASZ  = X2d(CVTEMLPE) - X2d(CVTEMLPS) + 1 /* size of EMLPA      */
  EMLPASZ  = EMLPASZ/1024                      /* convert to Kbytes  */
End
Else do /* no EMLPA */
  CVTEMLPS = 'N/A     '
  CVTEMLPE = 'N/A     '
  EMLPASZ  = 0
End
GDAECSA  = C2d(Storage(D2x(CVTGDA + 124),4)) /* start of ECSA addr   */
GDAECSAH = D2x(GDAECSA)                      /* display in hex       */
ECSAEND  = (GDAECSAS*1024) + GDAECSA - 1     /* end of ECSA          */
ECSAEND  = D2x(ECSAEND)                      /* display in hex       */
GDAEPVT  = C2d(Storage(D2x(CVTGDA + 168),4)) /* start of EPVT addr   */
GDAEPVTH = D2x(GDAEPVT)                      /* display in hex       */
EPVTEND  = (GDAEPVTS*1024*1024) + GDAEPVT - 1 /* end of EPVT         */
EPVTEND  = D2x(EPVTEND)                      /* display in hex       */
Queue 'Virtual Storage Map:'
Queue '          '
If VMAP = 'HIGHFIRST' then do
If Substr(FMIDNUM,4,4) >  6609 then ,
 Queue '     Storage Area     Start      End           Size' ,
       '     Used     Conv      HWM'
Else ,
 Queue '     Storage Area     Start      End           Size' ,
       '     Used     Conv'
Queue '          '
Queue '     Ext. Private    '     Right(GDAEPVTH,8,'0') ' ' ,
   Right(EPVTEND,8,'0')           Right(GDAEPVTS,8,' ')'M'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '         Ext. CSA    '     Right(GDAECSAH,8,'0') ' ' ,
   Right(ECSAEND,8,'0')           Right(GDAECSAS,8,' ')'K' ,
   Right(GDA_ECSA_ALLOC,8,' ')'K         ' ,
   Right(GDAECSAHWM,7,' ')'K'
Else ,
Queue '         Ext. CSA    '     Right(GDAECSAH,8,'0') ' ' ,
   Right(ECSAEND,8,'0')           Right(GDAECSAS,8,' ')'K' ,
   Right(GDA_ECSA_ALLOC,8,' ')'K'
Queue '        Ext. MLPA    '     Right(CVTEMLPS,8,'0') ' ' ,
   Right(CVTEMLPE,8,'0')          Right(EMLPASZ,8,' ')'K'
Queue '        Ext. FLPA    '     Right(CVTEFLPS,8,'0') ' ' ,
   Right(CVTEFLPE,8,'0')          Right(EFLPASZ,8,' ')'K'
Queue '        Ext. PLPA    '     Right(CVTEPLPS,8,'0') ' ' ,
   Right(CVTEPLPE,8,'0')          Right(EPLPASZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '         Ext. SQA    '     Right(GDAESQAH,8,'0') ' ' ,
   Right(ESQAEND,8,'0')           Right(GDAESQAS,8,' ')'K' ,
   Right(GDA_ESQA_ALLOC,8,' ')'K' Right(GDA_ECSA_CONV,7,' ')'K',
   Right(GDAESQAHWM,7,' ')'K'
Else ,
Queue '         Ext. SQA    '     Right(GDAESQAH,8,'0') ' ' ,
   Right(ESQAEND,8,'0')           Right(GDAESQAS,8,' ')'K' ,
   Right(GDA_ESQA_ALLOC,8,' ')'K' Right(GDA_ECSA_CONV,7,' ')'K'
Queue ' Ext. R/W Nucleus    '     Right(CVTERWNS,8,'0') ' ' ,
   Right(CVTERWNE,8,'0')          Right(ERWNUCSZ,8,' ')'K'
Queue ' Ext. R/O Nucleus    '     Right('1000000',8,'0') ' ' ,
   Right(CVTRONE,8,'0')           Right(RONUCSZA,8,' ')'K' ,
   '(Total' RONUCSZ'K)'
Queue '             16M line -----------------------------'
Queue '      R/O Nucleus    '     Right(CVTRONS,8,'0') ' ' ,
   Right('FFFFFF',8,'0')          Right(RONUCSZB,8,' ')'K',
   '(Spans 16M line)'
Queue '      R/W Nucleus    '     Right(CVTRWNS,8,'0') ' ' ,
   Right(CVTRWNE,8,'0')           Right(RWNUCSZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '              SQA    '     Right(GDASQAH,8,'0') ' ' ,
   Right(SQAEND,8,'0')            Right(GDASQASZ,8,' ')'K' ,
   Right(GDA_SQA_ALLOC,8,' ')'K'  Right(GDA_CSA_CONV,7,' ')'K' ,
   Right(GDASQAHWM,7,' ')'K'
Else ,
Queue '              SQA    '     Right(GDASQAH,8,'0') ' ' ,
   Right(SQAEND,8,'0')            Right(GDASQASZ,8,' ')'K' ,
   Right(GDA_SQA_ALLOC,8,' ')'K'  Right(GDA_CSA_CONV,7,' ')'K'
Queue '             PLPA    '     Right(CVTPLPAS,8,'0') ' ' ,
   Right(CVTPLPAE,8,'0')          Right(PLPASZ,8,' ')'K'
Queue '             FLPA    '     Right(CVTFLPAS,8,'0') ' ' ,
   Right(CVTFLPAE,8,'0')          Right(FLPASZ,8,' ')'K'
Queue '             MLPA    '     Right(CVTMLPAS,8,'0') ' ' ,
   Right(CVTMLPAE,8,'0')          Right(MLPASZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '              CSA    '     Right(GDACSAH,8,'0') ' ' ,
   Right(CSAEND,8,'0')            Right(GDACSASZ,8,' ')'K' ,
   Right(GDA_CSA_ALLOC,8,' ')'K         ' ,
   Right(GDACSAHWM,7,' ')'K'
Else ,
Queue '              CSA    '     Right(GDACSAH,8,'0') ' ' ,
   Right(CSAEND,8,'0')            Right(GDACSASZ,8,' ')'K' ,
   Right(GDA_CSA_ALLOC,8,' ')'K'
Queue '      Private V=V    '     Right(VVSTRT,8,'0') ' ' ,
   Right(VVEND,8,'0')             Right(LDASIZEA,8,' ')'K'
Queue '      Private V=R    '     Right(VRSTRT,8,'0') ' ' ,
   Right(VREND,8,'0')             Right(GDAVRSZ,8,' ')'K'
Queue '           System    '     Right(LDASTRTS,8,'0') ' ' ,
   Right(SYSEND,8,'0')            Right(LDASIZS,8,' ')'K'
If zARCH = 2 then ,
  Queue '              PSA     00000000   00001FFF        8K'
Else ,
  Queue '              PSA     00000000   00000FFF        4K'
End  /* if VMAP = 'HIGHFIRST'  */
Else do  /* VMAP <> 'HIGHFIRST'  */
If Substr(FMIDNUM,4,4) >  6609 then ,
 Queue '     Storage Area     Start      End           Size' ,
       '     Used     Conv      HWM'
Else ,
 Queue '     Storage Area     Start      End           Size' ,
       '     Used     Conv'
Queue '          '
If zARCH = 2 then ,
  Queue '              PSA     00000000   00001FFF        8K'
Else ,
  Queue '              PSA     00000000   00000FFF        4K'
Queue '           System    '     Right(LDASTRTS,8,'0') ' ' ,
   Right(SYSEND,8,'0')            Right(LDASIZS,8,' ')'K'
Queue '      Private V=R    '     Right(VRSTRT,8,'0') ' ' ,
   Right(VREND,8,'0')             Right(GDAVRSZ,8,' ')'K'
Queue '      Private V=V    '     Right(VVSTRT,8,'0') ' ' ,
   Right(VVEND,8,'0')             Right(LDASIZEA,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '              CSA    '     Right(GDACSAH,8,'0') ' ' ,
   Right(CSAEND,8,'0')            Right(GDACSASZ,8,' ')'K' ,
   Right(GDA_CSA_ALLOC,8,' ')'K         ' ,
   Right(GDACSAHWM,7,' ')'K'
Else ,
Queue '              CSA    '     Right(GDACSAH,8,'0') ' ' ,
   Right(CSAEND,8,'0')            Right(GDACSASZ,8,' ')'K' ,
   Right(GDA_CSA_ALLOC,8,' ')'K'
Queue '             MLPA    '     Right(CVTMLPAS,8,'0') ' ' ,
   Right(CVTMLPAE,8,'0')          Right(MLPASZ,8,' ')'K'
Queue '             FLPA    '     Right(CVTFLPAS,8,'0') ' ' ,
   Right(CVTFLPAE,8,'0')          Right(FLPASZ,8,' ')'K'
Queue '             PLPA    '     Right(CVTPLPAS,8,'0') ' ' ,
   Right(CVTPLPAE,8,'0')          Right(PLPASZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '              SQA    '     Right(GDASQAH,8,'0') ' ' ,
   Right(SQAEND,8,'0')            Right(GDASQASZ,8,' ')'K' ,
   Right(GDA_SQA_ALLOC,8,' ')'K'  Right(GDA_CSA_CONV,7,' ')'K' ,
   Right(GDASQAHWM,7,' ')'K'
Else ,
Queue '              SQA    '     Right(GDASQAH,8,'0') ' ' ,
   Right(SQAEND,8,'0')            Right(GDASQASZ,8,' ')'K' ,
   Right(GDA_SQA_ALLOC,8,' ')'K'  Right(GDA_CSA_CONV,7,' ')'K'
Queue '      R/W Nucleus    '     Right(CVTRWNS,8,'0') ' ' ,
   Right(CVTRWNE,8,'0')           Right(RWNUCSZ,8,' ')'K'
Queue '      R/O Nucleus    '     Right(CVTRONS,8,'0') ' ' ,
   Right('FFFFFF',8,'0')          Right(RONUCSZB,8,' ')'K',
   '(Spans 16M line)'
Queue '             16M line -----------------------------'
Queue ' Ext. R/O Nucleus    '     Right('1000000',8,'0') ' ' ,
   Right(CVTRONE,8,'0')           Right(RONUCSZA,8,' ')'K' ,
   '(Total' RONUCSZ'K)'
Queue ' Ext. R/W Nucleus    '     Right(CVTERWNS,8,'0') ' ' ,
   Right(CVTERWNE,8,'0')          Right(ERWNUCSZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '         Ext. SQA    '     Right(GDAESQAH,8,'0') ' ' ,
   Right(ESQAEND,8,'0')           Right(GDAESQAS,8,' ')'K' ,
   Right(GDA_ESQA_ALLOC,8,' ')'K' Right(GDA_ECSA_CONV,7,' ')'K',
   Right(GDAESQAHWM,7,' ')'K'
Else ,
Queue '         Ext. SQA    '     Right(GDAESQAH,8,'0') ' ' ,
   Right(ESQAEND,8,'0')           Right(GDAESQAS,8,' ')'K' ,
   Right(GDA_ESQA_ALLOC,8,' ')'K' Right(GDA_ECSA_CONV,7,' ')'K'
Queue '        Ext. PLPA    '     Right(CVTEPLPS,8,'0') ' ' ,
   Right(CVTEPLPE,8,'0')          Right(EPLPASZ,8,' ')'K'
Queue '        Ext. FLPA    '     Right(CVTEFLPS,8,'0') ' ' ,
   Right(CVTEFLPE,8,'0')          Right(EFLPASZ,8,' ')'K'
Queue '        Ext. MLPA    '     Right(CVTEMLPS,8,'0') ' ' ,
   Right(CVTEMLPE,8,'0')          Right(EMLPASZ,8,' ')'K'
If Substr(FMIDNUM,4,4) >  6609 then ,
Queue '         Ext. CSA    '     Right(GDAECSAH,8,'0') ' ' ,
   Right(ECSAEND,8,'0')           Right(GDAECSAS,8,' ')'K' ,
   Right(GDA_ECSA_ALLOC,8,' ')'K         ' ,
   Right(GDAECSAHWM,7,' ')'K'
Else ,
Queue '         Ext. CSA    '     Right(GDAECSAH,8,'0') ' ' ,
   Right(ECSAEND,8,'0')           Right(GDAECSAS,8,' ')'K' ,
   Right(GDA_ECSA_ALLOC,8,' ')'K'
Queue '     Ext. Private    '     Right(GDAEPVTH,8,'0') ' ' ,
   Right(EPVTEND,8,'0')           Right(GDAEPVTS,8,' ')'M'
End  /* else do (VMAP <> 'HIGHFIRST')  */
Return

PAGE:                /* Page Data Sets information sub-routine       */
Queue ' '
Queue 'Page Data Set Usage:'
Queue '  Type     Full   Slots  Dev   Volser  Data Set Name'
CVT      = C2d(Storage(10,4))              /* point to CVT           */
ASMVT    = C2d(Storage(D2x(CVT + 704),4))  /* point to ASMVT         */
ASMPART  = C2d(Storage(D2x(ASMVT + 8),4))  /* Pnt to Pag Act Ref Tbl */
PARTSIZE = C2d(Storage(D2x(ASMPART+4),4))  /* Tot number of entries  */
PARTDSNL = C2d(Storage(D2x(ASMPART+24),4)) /* Point to 1st pg dsn    */
PARTENTS = ASMPART+80                      /* Point to 1st parte     */
Do I = 1 to PARTSIZE
  If I > 1 then do
    PARTENTS = PARTENTS + 96
    PARTDSNL = PARTDSNL + 44
  End
  CHKINUSE = Storage(D2x(PARTENTS+9),1)    /* in use flag            */
  If bitand(CHKINUSE,'80'x) = '80'x then iterate /* not in use       */
  PGDSN    = Storage(D2x(PARTDSNL),44)     /* page data set name     */
  PGDSN    = Strip(PGDSN,T)                /* remove trailing blanks */
  PARETYPE = Storage(D2x(PARTENTS+8),1)    /* type flag              */
  Select
    When bitand(PARETYPE,'80'x) = '80'x then PGTYPE = ' PLPA    '
    When bitand(PARETYPE,'40'x) = '40'x then PGTYPE = ' COMMON  '
    When bitand(PARETYPE,'20'x) = '20'x then PGTYPE = ' DUPLEX  '
    When bitand(PARETYPE,'10'x) = '10'x then PGTYPE = ' LOCAL   '
    Otherwise PGTYPE = '??????'
  End  /* Select */
  If PGTYPE = ' LOCAL   ' then do
    PAREFLG1  = Storage(D2x(PARTENTS+9),1)    /* PARTE flags         */
    If bitand(PAREFLG1,'10'x) = '10'x then PGTYPE = ' LOCAL NV'
  End
  PAREUCBP = C2d(Storage(D2x(PARTENTS+44),4)) /* point to UCB        */
  PGUCB    = C2x(Storage(D2x(PAREUCBP+4),2))  /* UCB address         */
  PGVOL    = Storage(D2x(PAREUCBP+28),6)      /* UCB volser          */
  PARESZSL = C2d(Storage(D2x(PARTENTS+16),4)) /* total slots         */
  PARESZSL = Right(PARESZSL,7,' ')            /* ensure 7 digits     */
  PARESLTA = C2d(Storage(D2x(PARTENTS+20),4)) /* avail. slots        */
  PGFULL   = ((PARESZSL-PARESLTA) / PARESZSL) * 100 /* percent full  */
  PGFULL   = Format(PGFULL,3,2)               /* force 2 decimals    */
  PGFULL   = Left(PGFULL,3)                   /* keep intiger only   */
  Queue  ' 'PGTYPE' 'PGFULL'% 'PARESZSL'  'PGUCB' ' ,
         PGVOL'  'PGDSN
End  /* do I=1 to partsize */
Return

SMF:                 /* SMF Data Set information sub-routine         */
Queue ' '
Queue 'SMF Data Set Usage:'
Queue '  Name                      Volser   Size(Blks)  %Full  Status'
SMCAMISC = Storage(D2x(SMCA + 1),1)          /* misc. indicators     */
If bitand(SMCAMISC,'80'x) <> '80'x then do   /* smf active ??        */
  Queue '  *** SMF SYS1.MAN RECORDING NOT BEING USED ***'
  Return
End
SMCAFRDS = C2d(Storage(D2x(SMCA + 244),4))   /* point to first RDS   */
SMCALRDS = C2d(Storage(D2x(SMCA + 248),4))   /* point to last RDS    */
Do until SMCAFRDS = SMCALRDS    /* end loop when next rds ptr = last */
  RDSNAME  =  Strip(Storage(D2x(SMCAFRDS + 16),44))  /* smf dsn      */
  RDSVOLID = Storage(D2x(SMCAFRDS + 60),6)           /* smf volser   */
  RDSCAPTY = C2d(Storage(D2x(SMCAFRDS + 76),4))      /* size in blks */
  RDSNXTBL = C2d(Storage(D2x(SMCAFRDS + 80),4))      /* next avl blk */
  /* RDSPCT  = (RDSNXTBL / RDSCAPTY) * 100 */ /* not how mvs does it */
  RDSPCT   = Trunc((RDSNXTBL / RDSCAPTY) * 100) /* same as mvs disp. */
  RDSFLG1  = Storage(D2x(SMCAFRDS + 12),1)     /* staus flags        */
  Select
    When bitand(RDSFLG1,'10'x) = '10'x then RDSSTAT = 'FREE REQUIRED'
    When bitand(RDSFLG1,'08'x) = '08'x then RDSSTAT = 'DUMP REQUIRED'
    When bitand(RDSFLG1,'04'x) = '04'x then RDSSTAT = 'ALTERNATE'
    When bitand(RDSFLG1,'02'x) = '02'x then RDSSTAT = 'CLOSE PENDING'
    When bitand(RDSFLG1,'01'x) = '01'x then RDSSTAT = 'OPEN REQUIRED'
    When bitand(RDSFLG1,'00'x) = '00'x then RDSSTAT = 'ACTIVE'
    Otherwise RDSSTAT = '??????'
  End  /* Select */
  If (RDSSTAT = 'ACTIVE' | RDSSTAT = 'DUMP REQUIRED') , /* display   */
    & RDSPCT = 0 then RDSPCT = 1    /* %full the same way mvs does   */
  SMCAFRDS = C2d(Storage(D2x(SMCAFRDS + 4),4)) /* point to next RDS  */
  If Length(RDSNAME) < 26 then do
    Queue ' ' Left(RDSNAME,25,' ') RDSVOLID  Right(RDSCAPTY,11,' ') ,
              ' 'Format(RDSPCT,5,0) ' ' RDSSTAT
  End
  Else do
    Queue ' ' RDSNAME
    Queue copies(' ',27) RDSVOLID  Right(RDSCAPTY,11,' ') ,
              ' 'Format(RDSPCT,5,0) ' ' RDSSTAT
  End
End
Return

SUB:                 /* Subsystem information sub-routine            */
Arg SUBOPT
SSCVT    = C2d(Storage(D2x(JESCT+24),4))     /* point to SSCVT       */
SSCVT2   = SSCVT           /* save address for second loop           */
If SUBOPT <> 'FINDJES' then do
  Queue ' '
  Queue 'Subsystem Communications Vector Table:'
  Queue '  Name   Hex        SSCTADDR   SSCTSSVT' ,
        '  SSCTSUSE   SSCTSUS2   Status'
End /* if subopt */
Do until SSCVT = 0
  SSCTSNAM = Storage(D2x(SSCVT+8),4)         /* subsystem name       */
  SSCTSSVT = C2d(Storage(D2x(SSCVT+16),4))   /* subsys vect tbl ptr  */
  SSCTSUSE = C2d(Storage(D2x(SSCVT+20),4))   /* SSCTSUSE pointer     */
  SSCTSUS2 = C2d(Storage(D2x(SSCVT+28),4))   /* SSCTSUS2 pointer     */
  If SUBOPT = 'FINDJES' & SSCTSNAM = JESPJESN then do
     JESSSVT  = SSCTSSVT   /* save SSVTSSVT for "version" section    */
                           /* this points to JES3 Subsystem Vector   */
                           /* Table, mapped by IATYSVT               */
     JESSUSE  = SSCTSUSE   /* save SSCTSUSE for "version" section    */
                           /* this points to version for JES2        */
     JESSUS2  = SSCTSUS2   /* save SSCTSUS2 for "version" section    */
                           /* this points to $HCCT for JES2          */
     Leave  /* found JES info for version section, exit loop */
  End /* if subopt */
  SSCTSNAX = C2x(SSCTSNAM)    /* chg to EBCDIC for non-display chars */
  Call XLATE_NONDISP SSCTSNAM /* translate non display chars         */
  SSCTSNAM = RESULT           /* result from XLATE_NONDISP           */
  If SSCTSSVT = 0 then SSCT_STAT = 'Inactive'
    Else SSCT_STAT = 'Active'
  If SUBOPT <> 'FINDJES' then do
    Queue ' ' SSCTSNAM ' ' SSCTSNAX  ,
          ' ' Right(D2x(SSCVT),8,0)    ' ' Right(D2x(SSCTSSVT),8,0) ,
          ' ' Right(D2x(SSCTSUSE),8,0) ' ' Right(D2x(SSCTSUS2),8,0) ,
          ' ' SSCT_STAT ' '
  End /* if SUBOPT */
 /*SSCTSSID = C2d(Storage(D2x(SSCVT+13),1)) */ /* subsys identifier  */
 /*If bitand(SSCTSSID,'02'x) = '02'x then JESPJESN = 'JES2' */
 /*If bitand(SSCTSSID,'03'x) = '03'x then JESPJESN = 'JES3'*/
  SSCVT    = C2d(Storage(D2x(SSCVT+4),4))    /* next sscvt or zero   */
End /* do until sscvt = 0 */
If SUBOPT <> 'FINDJES' then do
  Queue ' '
  Queue 'Supported Subsystem Function Codes:'
  Do until SSCVT2 = 0 /* 2nd loop for function codes                 */
    SSCTSNAM = Storage(D2x(SSCVT2+8),4)        /* subsystem name     */
    SSCTSSVT = C2d(Storage(D2x(SSCVT2+16),4)) /* subsys vect tbl ptr */
    SSCTSNAX = C2x(SSCTSNAM)  /* chg to EBCDIC for non-display chars */
    Call XLATE_NONDISP SSCTSNAM /* translate non display chars       */
    SSCTSNAM = RESULT           /* result from XLATE_NONDISP         */
    Queue SSCTSNAM '(X''' || SSCTSNAX || ''')'
    If SSCTSSVT <> 0 then do
      SSVTFCOD = SSCTSSVT + 4                  /* pt to funct. matrix*/
      SSFUNCTB = Storage(D2X(SSVTFCOD),255)    /* function matrix    */
      TOTFUNC = 0       /* counter for total functions per subsystem */
      Drop FUNC.        /* init stem to null for saved functions     */
      Do SUPFUNC = 1 TO 255
        If Substr(SSFUNCTB,SUPFUNC,1) <> '00'x then do /* supported? */
          TOTFUNC = TOTFUNC + 1 /* tot functions for this subsystem  */
          FUNC.TOTFUNC = SUPFUNC  /* save function in stem           */
        End
      End /* do supfunc */
      /***************************************************************/
      /* The following code is used to list the supported functions  */
      /* on a single line by ranges. For example: 1-10,13,18-30,35   */
      /***************************************************************/
      If TOTFUNC >= 1 then do   /* begin loop to list function codes */
        ALLCODES = ''                   /* init var to nulls         */
        NEWRANGE = 'YES'                /* init newrange flag to YES */
        FIRSTRNG = 'YES'                /* init firstrng flag to YES */
        Do FCODES = 1 to TOTFUNC        /* loop though codes         */
          CHKNEXT = FCODES + 1          /* stem var to chk next code */
          If FUNC.FCODES + 1 = FUNC.CHKNEXT then do  /* next matches */
            If NEWRANGE = 'YES' & FIRSTRNG = 'YES' then do /* first  */
              ALLCODES =  ALLCODES || FUNC.FCODES || '-'   /* in new */
              NEWRANGE = 'NO'                    /* range - seperate */
              FIRSTRNG = 'NO'                    /* with a dash      */
              Iterate                            /* get next code    */
            End /* if newrange = 'yes' & firstrng = 'yes'            */
            If NEWRANGE = 'YES' & FIRSTRNG = 'NO' then do /* next    */
              ALLCODES =  ALLCODES || FUNC.FCODES  /* matches, but   */
              NEWRANGE = 'NO'   /* is not the first, don't add dash  */
              Iterate                            /* get next code    */
            End /* if newrange = 'yes' & firstrng = 'no'             */
            Else iterate  /* same range + not first - get next code  */
          End /* func.fcodes + 1 */
          If FCODES = TOTFUNC then , /* next doesn't match and this  */
            ALLCODES =  ALLCODES || FUNC.FCODES  /* is the last code */
          Else do /* next code doesn't match - seperate with comma   */
            ALLCODES =  ALLCODES || FUNC.FCODES || ','
            NEWRANGE = 'YES'         /* re-init newrange flag to YES */
            FIRSTRNG = 'YES'         /* re-init firstrng flag to YES */
          End
        End /* do fcodes = 1 to totfunc */
        Queue '  Codes:' ALLCODES
      End /* if totfunc >= 1 */
    End
    Else queue '  *Inactive*'
    SSCVT2   = C2d(Storage(D2x(SSCVT2+4),4))   /* next sscvt or zero */
  End /* do until sscvt2 = 0 */
End /* if subopt <> 'findjes' */
Return

ASID:                /* ASVT Usage sub-routine                       */
Queue ' '
CVTASVT  = C2d(Storage(D2x(CVT+556),4))     /* point to ASVT         */
ASVTMAXU = C2d(Storage(D2x(CVTASVT+516),4)) /* max number of entries */
ASVTMAXI = C2d(Storage(D2x(CVTASVT+500),4)) /* MAXUSERS from ASVT    */
ASVTAAVT = C2d(Storage(D2x(CVTASVT+480),4)) /* free slots in ASVT    */
ASVTSTRT = C2d(Storage(D2x(CVTASVT+492),4)) /* RSVTSTRT from ASVT    */
ASVTAST  = C2d(Storage(D2x(CVTASVT+484),4)) /* free START/SASI       */
ASVTNONR = C2d(Storage(D2x(CVTASVT+496),4)) /* RSVNONR  from ASVT    */
ASVTANR  = C2d(Storage(D2x(CVTASVT+488),4)) /* free non-reusable     */
Queue 'ASID Usage Summary from the ASVT:'
Queue '  Maximum number of ASIDs:' Right(ASVTMAXU,5,' ')
Queue '                          '
Queue '    MAXUSER from IEASYSxx:' Right(ASVTMAXI,5,' ')
Queue '             In use ASIDs:' Right(ASVTMAXI-ASVTAAVT,5,' ')
Queue '          Available ASIDs:' Right(ASVTAAVT,5,' ')
Queue '                          '
Queue '    RSVSTRT from IEASYSxx:' Right(ASVTSTRT,5,' ')
Queue '           RSVSTRT in use:' Right(ASVTSTRT-ASVTAST,5,' ')
Queue '        RSVSTRT available:' Right(ASVTAST,5,' ')
Queue '                          '
Queue '    RSVNONR from IEASYSxx:' Right(ASVTNONR,5,' ')
Queue '           RSVNONR in use:' Right(ASVTNONR-ASVTANR,5,' ')
Queue '        RSVNONR available:' Right(ASVTANR,5,' ')
Return

XLATE_NONDISP:       /* translate non-display characters to a "."    */
Arg XLATEPRM
XLATELEN = Length(XLATEPRM) /* length of parm passed to routine      */
Do I = 1 to XLATELEN                      /* check each byte for     */
  If (Substr(XLATEPRM,I,1) > '00'x & ,    /* non-display characters  */
    Substr(XLATEPRM,I,1) < '40'x ) | ,    /* and replace each        */
    Substr(XLATEPRM,I,1) = 'FF'x  then ,  /* character that          */
    XLATEPRM = OVERLAY('.',XLATEPRM,I)    /* is non-displayable      */
End                                       /* with a period (.)       */
Return XLATEPRM

STORAGE_GDA_LDA:     /* GDA/LDA Storage values sub-routine           */
ASCB     = C2d(Storage(224,4))               /* point to cur ASCB    */
ASCBLDA  = C2d(Storage(D2x(ASCB + 48),4))    /* point to LDA         */
CVTGDA   = C2d(Storage(D2x(CVT + 560),4))    /* point to GDA         */
LDASTRTA = Storage(D2x(ASCBLDA + 60),4)      /* point to V=V start   */
LDASTRTA = C2x(LDASTRTA)                     /* display in hex       */
LDASIZEA = C2d(Storage(D2x(ASCBLDA + 64),4)) /* point to V=V size    */
LDASIZEA = LDASIZEA/1024                     /* convert to Kbytes    */
LDASTRTS = Storage(D2x(ASCBLDA + 92),4)      /* pt. to sysarea start */
LDASTRTS = C2x(LDASTRTS)                     /* display in hex       */
LDASIZS  = C2d(Storage(D2x(ASCBLDA + 96),4)) /* pt. to sysarea size  */
LDASIZS  = LDASIZS/1024                      /* convert to Kbytes    */
GDAPVTSZ = C2d(Storage(D2x(CVTGDA + 164),4)) /* point to MAX PVT<16M */
GDAPVTSZ = GDAPVTSZ/1024                     /* convert to Kbytes    */
GDAEPVTS = C2d(Storage(D2x(CVTGDA + 172),4)) /* point to MAX PVT>16M */
GDAEPVTS = GDAEPVTS/1024/1024                /* convert to Mbytes    */
GDACSASZ = C2d(Storage(D2x(CVTGDA + 112),4)) /* point to CSA<16M     */
GDACSASZ = GDACSASZ/1024                     /* convert to Kbytes    */
GDAECSAS = C2d(Storage(D2x(CVTGDA + 128),4)) /* point to CSA>16M     */
GDAECSAS = GDAECSAS/1024                     /* convert to Kbytes    */
GDASQASZ = C2d(Storage(D2x(CVTGDA + 148),4)) /* point to SQA<16M     */
GDASQASZ = GDASQASZ/1024                     /* convert to Kbytes    */
GDAESQAS = C2d(Storage(D2x(CVTGDA + 156),4)) /* point to SQA>16M     */
GDAESQAS = GDAESQAS/1024                     /* convert to Kbytes    */
GDAVRSZ  = C2d(Storage(D2x(CVTGDA + 196),4)) /* point to V=R global  */
GDAVRSZ  = GDAVRSZ/1024                      /* convert to Kbytes    */
GDAVREGS = C2d(Storage(D2x(CVTGDA + 200),4)) /* point to V=R default */
GDAVREGS = GDAVREGS/1024                     /* convert to Kbytes    */
GDA_CSA_ALLOC  = C2d(Storage(D2x(CVTGDA + 432),4)) /* CSA amt alloc  */
GDA_CSA_ALLOC  = Format(GDA_CSA_ALLOC/1024,,0)     /* conv to Kbytes */
GDA_ECSA_ALLOC = C2d(Storage(D2x(CVTGDA + 436),4)) /* ECSA amt alloc */
GDA_ECSA_ALLOC = Format(GDA_ECSA_ALLOC/1024,,0)    /* conv to Kbytes */
GDA_SQA_ALLOC  = C2d(Storage(D2x(CVTGDA + 440),4)) /* SQA amt alloc  */
GDA_SQA_ALLOC  = Format(GDA_SQA_ALLOC/1024,,0)     /* conv to Kbytes */
GDA_ESQA_ALLOC = C2d(Storage(D2x(CVTGDA + 444),4)) /* ESQA amt alloc */
GDA_ESQA_ALLOC = Format(GDA_ESQA_ALLOC/1024,,0)    /* conv to Kbytes */
GDA_CSA_CONV   = C2d(Storage(D2x(CVTGDA + 448),4)) /* CSA => SQA amt */
GDA_CSA_CONV   = Format(GDA_CSA_CONV/1024,,0)      /* conv to Kbytes */
GDA_ECSA_CONV  = C2d(Storage(D2x(CVTGDA + 452),4)) /* ECSA=>ESQA amt */
GDA_ECSA_CONV  = Format(GDA_ECSA_CONV/1024,,0)     /* conv to Kbytes */
/*********************************************************************/
/* High Water Marks for SQA/ESQA/CSA/ECSA added in OS/390 R10        */
/*********************************************************************/
If Substr(FMIDNUM,4,4) >  6609 then do       /* OS/390 R10 or above  */
  GDASQAHWM  = C2d(Storage(D2x(CVTGDA + 536),4))   /* SQA HWM        */
  GDASQAHWM  = Format(GDASQAHWM/1024,,0)           /* conv to Kbytes */
  GDAESQAHWM = C2d(Storage(D2x(CVTGDA + 540),4))   /* ESQA HWM       */
  GDAESQAHWM = Format(GDAESQAHWM/1024,,0)          /* conv to Kbytes */
  GDACSAHWM  = C2d(Storage(D2x(CVTGDA + 544),4))   /* SQA HWM        */
  GDACSAHWM  = Format(GDACSAHWM/1024,,0)           /* conv to Kbytes */
  GDAECSAHWM = C2d(Storage(D2x(CVTGDA + 548),4))   /* ESQA HWM       */
  GDAECSAHWM = Format(GDAECSAHWM/1024,,0)          /* conv to Kbytes */
End
Return

EXTRACT_SYSPARMS:    /* Extract IEASYSxx values from the IPA         */
Parse arg IEASPARM
IEASPARM = Strip(IEASPARM,T)                 /* remove trailing blnks*/
If IEASPARM = '<notdef>' then return         /*"blank" parm in IHAIPA*/
If Substr(FMIDNUM,4,4) >= 7708 then ,        /* if >= z/OS 1.5 then  */
  If Pos('ILM',IEASPARM) <> 0  then return   /* remove ILM parms     */
IPAOFF = ((I-1) * 8)                         /* offset to next entry */
IPASTOR = D2x(ECVTIPA + 2152 + IPAOFF)       /* point to PDE addr    */
IPAPDE  = C2x(Storage((IPASTOR),8))          /* point to PDE         */
If IPAPDE = 0 then return   /* parm not specified and has no default */
TOTPRMS = TOTPRMS + 1    /* tot num of specified or defaulted parms  */
IPAADDR = Substr(IPAPDE,1,8)                 /* PARM address         */
IPALEN  = X2d(Substr(IPAPDE,9,4))            /* PARM length          */
IPAPRM  = Storage((IPAADDR),IPALEN)          /* PARM                 */
IPASRC  = Substr(IPAPDE,13,4)                /* PARM source          */
If X2d(IPASRC) = 65535 then PRMSRC = 'Operator'   /* operator parm   */
Else
  If X2d(IPASRC) = 0     then PRMSRC = 'Default'  /* default  parm   */
Else
  PRMSRC = 'IEASYS' || X2c(IPASRC)           /* IEASYSxx parm        */
PRMLINE = '    'IEASPARM'='IPAPRM
PRMLINE.TOTPRMS = IEASPARM PRMLINE PRMSRC
PRMLINE.0 = TOTPRMS
Return

BUILD_IPAPDETB:      /* Build table for lookup for IPA values        */
NUM=1
IPAPDETB.NUM = 'ALLOC   ' ; NUM = NUM + 1
IPAPDETB.NUM = 'APF     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'APG     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'BLDL    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'BLDLF   ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CLOCK   ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CLPA    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CMB     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CMD     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CON     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CONT    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'COUPLE  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CPQE    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CSA     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CSCBLOC ' ; NUM = NUM + 1
IPAPDETB.NUM = 'CVIO    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'DEVSUP  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'DIAG    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'DUMP    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'DUPLEX  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'EXIT    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'FIX     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'GRS     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'GRSCNF  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'GRSRNL  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'ICS     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'IOS     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'IPS     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LNK     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LNKAUTH ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LOGCLS  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LOGLMT  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LOGREC  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'LPA     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'MAXCAD  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'MAXUSER ' ; NUM = NUM + 1
IPAPDETB.NUM = 'MLPA    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'MSTRJCL ' ; NUM = NUM + 1
IPAPDETB.NUM = 'NONVIO  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'NSYSLX  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'NUCMAP  ' ; NUM = NUM + 1
If Substr(FMIDNUM,4,4) >= 6603 then do
   IPAPDETB.NUM = 'OMVS    ' ; NUM = NUM + 1
End
Else do
   IPAPDETB.NUM = 'RESERVED' ; NUM = NUM + 1
End
IPAPDETB.NUM = 'OPI     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'OPT     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PAGE-OPR' ; NUM = NUM + 1
IPAPDETB.NUM = 'PAGE    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PAGNUM  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PAGTOTL ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PAK     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PLEXCFG ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PROD    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PROG    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'PURGE   ' ; NUM = NUM + 1
IPAPDETB.NUM = 'RDE     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'REAL    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'RER     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'RSU     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'RSVNONR ' ; NUM = NUM + 1
IPAPDETB.NUM = 'RSVSTRT ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SCH     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SMF     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SMS     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SQA     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SSN     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SVC     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SWAP    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SYSNAME ' ; NUM = NUM + 1
IPAPDETB.NUM = 'SYSP    ' ; NUM = NUM + 1
IPAPDETB.NUM = 'VAL     ' ; NUM = NUM + 1
IPAPDETB.NUM = 'VIODSN  ' ; NUM = NUM + 1
IPAPDETB.NUM = 'VRREGN  ' ; NUM = NUM + 1
If Substr(FMIDNUM,4,4) >= 6604 then do
   IPAPDETB.NUM = 'RTLS    ' ; NUM = NUM + 1
End
If Substr(FMIDNUM,4,4) >= 6608 then do  /* added by APAR OW44581 */
   IPAPDETB.NUM = 'UNI     ' ; NUM = NUM + 1
End
If Substr(FMIDNUM,4,4) >  7703 then do
   IPAPDETB.NUM = 'ILMLIB  ' ; NUM = NUM + 1
   IPAPDETB.NUM = 'ILMMODE ' ; NUM = NUM + 1
End
If Substr(FMIDNUM,4,4) >= 7706 then do
   IPAPDETB.NUM = 'IKJTSO  ' ; NUM = NUM + 1
   IPAPDETB.NUM = 'LICENSE ' ; NUM = NUM + 1
End
If Substr(FMIDNUM,4,4) >= 7708 then do
   IPAPDETB.NUM = '<notdef>' ; NUM = NUM + 1 /*"blank" def in IHAIPA */
   IPAPDETB.NUM = 'HVSHARE ' ; NUM = NUM + 1
   IPAPDETB.NUM = 'ILM     ' ; NUM = NUM + 1
 /********************************************************************/
 /* If you have a z/OS 1.5 or z/OS 1.6 system without OA09649, you   */
 /* may have to delete the next 3 lines of code.                     */
 /********************************************************************/
   IPAPDETB.NUM = '<notdef>' ; NUM = NUM + 1 /*"blank" def in IHAIPA */
   IPAPDETB.NUM = '<notdef>' ; NUM = NUM + 1 /*"blank" def in IHAIPA */
   IPAPDETB.NUM = 'PRESCPU ' ; NUM = NUM + 1 /* added by OA09649 */
End
If Substr(FMIDNUM,4,4) >= 7720 then do
   NUM = NUM-3
   IPAPDETB.NUM = 'DRMODE  ' ; NUM = NUM + 1
   IPAPDETB.NUM = 'CEE     ' ; NUM = NUM + 1
   IPAPDETB.NUM = 'PRESCPU ' ; NUM = NUM + 1
End
IPAPDETB.0 = NUM-1
Return

SPLIT_IPA_PAGE: /* Split up page data set parms to multiple lines */
TOT_IPALINES = 0
Do SPLIT = 1 to PRMLINE.0
   TOT_IPALINES = TOT_IPALINES+1    /* add one total lines    */
   IPA_PDE = Word(PRMLINE.SPLIT,1)  /* keyword                */
   IPA_PRM = Word(PRMLINE.SPLIT,2)  /* value                  */
   IPA_SRC = Word(PRMLINE.SPLIT,3)  /* IEASYSxx, dlft, or OPR */
   IPA_LEN = Length(IPA_PRM)
  If IPA_PDE = 'NONVIO' | IPA_PDE = 'PAGE' | ,
     IPA_PDE = 'PAGE-OPR' | IPA_PDE = 'SWAP' then do
    MORE  = 'YES' /* init flag for more subparms */
    FIRST = 'YES' /* init flag for first subparm */
    SPLITPOS = 1
    Do until MORE = 'NO'
      SPLITPOS = Pos(',',IPA_PRM)
      If SPLITPOS = 0 then do
        If FIRST = 'YES' then do
          IPALINE.TOT_IPALINES = '    'IPA_PRM || ','
          IPALINE.TOT_IPALINES = ,
            Overlay(IPA_SRC,IPALINE.TOT_IPALINES,68)
        End
        Else do
          MBLNK = ''
          If IPA_PDE = 'NONVIO' then MBLNK = '  '     /* align   */
          If IPA_PDE = 'PAGE-OPR' then MBLNK = '    ' /* align   */
          IPALINE.TOT_IPALINES = MBLNK'          'IPA_PRM || ','
          IPALINE.TOT_IPALINES = ,
            Overlay(IPA_SRC,IPALINE.TOT_IPALINES,68)
        End
        MORE = 'NO'  /* no more subparms */
      End /* if SPLITPOS = 0 */
      Else do
        IPAPRM_SPLIT = Substr(IPA_PRM,1,SPLITPOS)
        If FIRST = 'YES' then IPALINE.TOT_IPALINES = '    'IPAPRM_SPLIT
          Else do
            MBLNK = ''
            If IPA_PDE = 'NONVIO' then MBLNK = '  '     /* align   */
            If IPA_PDE = 'PAGE-OPR' then MBLNK = '    ' /* align   */
            IPALINE.TOT_IPALINES = MBLNK'          'IPAPRM_SPLIT
          End
        IPA_PRM  = Substr(IPA_PRM,SPLITPOS+1,IPA_LEN-SPLITPOS)
        IPA_LEN =  Length(IPA_PRM)
        TOT_IPALINES = TOT_IPALINES+1  /* add one total lines */
        FIRST = 'NO'
      End
    End  /* do until more=no */
  End
  Else do
    IPALINE.TOT_IPALINES = '    'IPA_PRM || ','
    IPALINE.TOT_IPALINES = Overlay(IPA_SRC,IPALINE.TOT_IPALINES,68)
  End
End
Return

SORT_IPA: Procedure expose PRMLINE.
/* bubble sort the IPA list */
SORT_DONE = 0
SORT_RECS = PRMLINE.0
Do while SORT_DONE = 0
  SORT_DONE = 1
  Do I = 1 to SORT_RECS - 1
    J = I + 1
    If PRMLINE.I > PRMLINE.J then do
      SORT_DONE = 0
      TEMP_SORT = PRMLINE.J
      PRMLINE.J = PRMLINE.I
      PRMLINE.I = TEMP_SORT
    End /* if */
  End /* do i=1 to sort_recs */
  SORT_RECS = SORT_RECS - 1
End /* do while */
Return

BROWSE_ISPF:         /* Browse output if ISPF is active              */
Address ISPEXEC "CONTROL ERRORS RETURN"
Address TSO
prefix = sysvar('SYSPREF')        /* tso profile prefix            */
uid    = sysvar('SYSUID')         /* tso userid                    */
If prefix = '' then prefix = uid  /* use uid if null prefix        */
If prefix <> '' & prefix <> uid then /* different prefix than uid  */
   prefix = prefix || '.' || uid /* use  prefix.uid                */
ddnm1 = 'DD'||random(1,99999)   /* choose random ddname            */
ddnm2 = 'DD'||random(1,99999)   /* choose random ddname            */
junk = msg(off)
"ALLOC FILE("||ddnm1||") UNIT(SYSALLDA) NEW TRACKS SPACE(2,1) DELETE",
      " REUSE LRECL(80) RECFM(F B) BLKSIZE(3120)"
"ALLOC FILE("||ddnm2||") UNIT(SYSALLDA) NEW TRACKS SPACE(1,1) DELETE",
      " REUSE LRECL(80) RECFM(F B) BLKSIZE(3120) DIR(1)",
      " DA('"||prefix||".IPLINFO." ||ddnm2|| ".ISPPLIB')"
junk = msg(on)
Newstack
/*************************/
/* IPLINFOP Panel source */
/*************************/
If Substr(ZENVIR,6,1) >= 4 then
  Queue ")PANEL KEYLIST(ISRSPBC,ISR)"
Queue ")ATTR"
Queue "  _ TYPE(INPUT)   INTENS(HIGH) COLOR(TURQ) CAPS(OFF)" ,
      "FORMAT(&MIXED)"
Queue "  | AREA(DYNAMIC) EXTEND(ON)   SCROLL(ON)"
Queue "  + TYPE(TEXT)    INTENS(LOW)  COLOR(BLUE)"
Queue "  @ TYPE(TEXT)    INTENS(LOW)  COLOR(TURQ)"
Queue "  % TYPE(TEXT)    INTENS(HIGH) COLOR(GREEN)"
Queue "  ! TYPE(OUTPUT)  INTENS(HIGH) COLOR(TURQ) PAD(-)"
Queue " 01 TYPE(DATAOUT) INTENS(LOW)"
Queue " 02 TYPE(DATAOUT) INTENS(HIGH)"
Queue " 0B TYPE(DATAOUT) INTENS(HIGH) FORMAT(DBCS)"
Queue " 0C TYPE(DATAOUT) INTENS(HIGH) FORMAT(EBCDIC)"
Queue " 0D TYPE(DATAOUT) INTENS(HIGH) FORMAT(&MIXED)"
Queue " 10 TYPE(DATAOUT) INTENS(LOW)  FORMAT(DBCS)"
Queue " 11 TYPE(DATAOUT) INTENS(LOW)  FORMAT(EBCDIC)"
Queue " 12 TYPE(DATAOUT) INTENS(LOW)  FORMAT(&MIXED)"
Queue ")BODY EXPAND(//)"
Queue "%BROWSE  @&ZTITLE  / /  %Line!ZLINES  %Col!ZCOLUMS+"
Queue "%Command ===>_ZCMD / /           %Scroll ===>_Z   +"
Queue "|ZDATA ---------------/ /-------------------------|"
Queue "|                     / /                         |"
Queue "| --------------------/-/-------------------------|"
Queue ")INIT"
Queue "  .HELP = IPLINFOH"
Queue "  .ZVARS = 'ZSCBR'"
Queue "  &ZTITLE = 'z/OS -- MVS Utilities - IPLINFO'"
Queue "  &MIXED = MIX"
Queue "  IF (&ZPDMIX = N)"
Queue "   &MIXED = EBCDIC"
Queue "  VGET (ZSCBR) PROFILE"
Queue "  IF (&ZSCBR = ' ')"
Queue "   &ZSCBR = 'CSR'"
Queue ")REINIT"
Queue "  .HELP = IPLINFOH"
Queue "  REFRESH(ZCMD,ZSCBR,ZDATA,ZLINES,ZCOLUMS)"
Queue ")PROC"
Queue "  &ZCURSOR = .CURSOR"
Queue "  &ZCSROFF = .CSRPOS"
Queue "  &ZLVLINE = LVLINE(ZDATA)"
Queue "  VPUT (ZSCBR) PROFILE"
Queue ")END"
/*                                    */
"ALLOC FILE(IPLINFOP) SHR REUSE",
      " DA('"||prefix||".IPLINFO." ||ddnm2|| ".ISPPLIB(IPLINFOP)')"
"EXECIO" Queued() "DISKW IPLINFOP (FINIS"
/* "FREE FI(IPLINFOP)" */
Delstack
Newstack
/*************************/
/* IPLINFOH Panel source */
/*************************/
If Substr(ZENVIR,6,1) >= 4 then
  Queue ")PANEL KEYLIST(ISRSPBC,ISR)"
Queue ")ATTR DEFAULT(!+_)"
Queue "  _ TYPE(INPUT)   INTENS(HIGH) COLOR(TURQ) CAPS(OFF)" ,
      "FORMAT(&MIXED)"
Queue "  + TYPE(TEXT)    INTENS(LOW)  COLOR(BLUE)"
Queue "  @ TYPE(TEXT)    INTENS(LOW)  COLOR(TURQ)"
Queue "  ! TYPE(TEXT)    INTENS(HIGH) COLOR(GREEN)"
Queue ")BODY EXPAND(//)"
Queue "!HELP    @&ZTITLE  / / "
Queue "!Command ===>_ZCMD / / "
Queue "+                                                              "
Queue "+EXECUTION SYNTAX:  !TSO %IPLINFO <option>                     "
Queue "+                                                              "
Queue "+VALID OPTIONS ARE 'ALL' (default), 'IPL', 'VERSION'," ||,
      " 'STOR', 'CPU', 'IPA', "
Queue "+                  'SYMBOLS', 'VMAP', 'PAGE', 'SMF', " ||,
      "'SUB' and 'ASID'"
Queue "+Examples:                                                     "
Queue "! TSO %IPLINFO        +(Display all Information)               "
Queue "! TSO %IPLINFO IPL    +(Display IPL Information)               "
Queue "! TSO %IPLINFO VERSION+(Display Version Information)           "
Queue "! TSO %IPLINFO STOR   +(Display Storage Information)           "
Queue "! TSO %IPLINFO CPU    +(Display CPU Information)               "
Queue "! TSO %IPLINFO IPA    +(Display Initialization Information)    "
Queue "! TSO %IPLINFO SYMBOLS+(Display Static System Symbols)         "
Queue "! TSO %IPLINFO VMAP   +(Display a Virtual Storage Map)         "
Queue "! TSO %IPLINFO PAGE   +(Display Page Data Set Usage",
                              "Information)"
Queue "! TSO %IPLINFO SMF    +(Display SMF Data Set Usage Information)"
Queue "! TSO %IPLINFO SUB    +(Display Subsystem Information)         "
Queue "! TSO %IPLINFO ASID   +(Display ASID Usage Information)        "
Queue "+                                                              "
Queue "@&ADLINE"
Queue ")INIT"
Queue "  .HELP = ISR10000"
Queue "  &ZTITLE = 'Mark''s MVS Utilities - IPLINFO'"
Queue "  &L1 = 'Mark''s MVS Utilities -'"
Queue "  &L2 = 'http://home.flash.net/~mzelden/mvsutil.html'"
Queue "  &ADLINE  = '&L1 &L2'"
Queue "  &MIXED = MIX"
Queue "  IF (&ZPDMIX = N)"
Queue "   &MIXED = EBCDIC"
Queue ")END"
/*                                    */
"ALLOC FILE(IPLINFOP) SHR REUSE",
      " DA('"||prefix||".IPLINFO." ||ddnm2|| ".ISPPLIB(IPLINFOH)')"
"EXECIO" Queued() "DISKW IPLINFOP (FINIS"
"FREE FI(IPLINFOP)"
Delstack
"EXECIO" Queued() "DISKW" ddnm1 "(FINIS"
zerrsm  = 'IPLINFO' LASTUPD
zerrlm  = 'IPLINFO -' OPTION 'option.' ,
          'Last updated on' LASTUPD ||'. Written by' ,
          'Mark Zelden. Mark''s MVS Utilities -' ,
          'http://home.flash.net/~mzelden/mvsutil.html'
zerralrm = 'NO'        /* msg - no alarm */
zerrhm   = 'IPLINFOH'  /* help panel */
address ISPEXEC "LIBDEF ISPPLIB LIBRARY ID("||ddnm2||") STACK"
address ISPEXEC "SETMSG MSG(ISRZ002)"
address ISPEXEC "LMINIT DATAID(TEMP) DDNAME("||ddnm1||")"
address ISPEXEC "BROWSE DATAID("||temp") PANEL(IPLINFOP)"
address ISPEXEC "LMFREE DATAID("||temp")"
address ISPEXEC "LIBDEF ISPPLIB"
junk = msg(off)
"FREE FI("||ddnm1||")"
"FREE FI("||ddnm2||")"
Return

/* rexx */
RDATE:
/*                                       */
/* AUTHOR: Mark Zelden                   */
/*                                       */
/************************************************/
/* Convert MM DD YYYY , YYYY DDD, or NNNNN to   */
/* standard date output that includes the day   */
/* of the week and the number of days (NNNNN)   */
/* from January 1, 1900. This is not the same   */
/* as the Century date! Valid input dates range */
/* from 01/01/1900 through 12/31/2172.          */
/*                                              */
/* A parm of "TODAY" can also be passed to      */
/* the date conversion routine.                 */
/* MM DD YYYY can also be specifed as           */
/* MM/DD/YYYY or MM-DD-YYYY.                    */
/*                                              */
/* The output format is always as follows:      */
/*      MM/DD/YYYY.JJJ NNNNN WEEKDAY            */
/*                                              */
/* The above value will be put in the special   */
/* REXX variable "RESULT"                       */
/* example: CALL RDATE TODAY                    */
/* example: CALL RDATE 1996 300                 */
/* example: CALL RDATE 10 26 1996               */
/* example: CALL RDATE 10/26/1996               */
/* example: CALL RDATE 10-26-1996               */
/* example: CALL RDATE 35363                    */
/* result:  10/26/1996.300 35363 Saturday       */
/************************************************/
arg P1 P2 P3

If Pos('/',P1) <> 0 | Pos('-',P1) <> 0 then do
  PX =  Translate(P1,'  ','/-')
  Parse var PX P1 P2 P3
End

JULTBL = '000031059090120151181212243273304334'
DAY.0 = 'Sunday'
DAY.1 = 'Monday'
DAY.2 = 'Tuesday'
DAY.3 = 'Wednesday'
DAY.4 = 'Thursday'
DAY.5 = 'Friday'
DAY.6 = 'Saturday'

Select
  When P1 = 'TODAY' then do
    P1 = Substr(date('s'),5,2)
    P2 = Substr(date('s'),7,2)
    P3 = Substr(date('s'),1,4)
    call CONVERT_MDY
    call THE_END
  end
  When P2 = '' & P3 = '' then do
    call CONVERT_NNNNN
    call THE_END
  end
  When P3 = '' then do
    call CONVERT_JDATE
    call DOUBLE_CHECK
    call THE_END
  end
  otherwise do
    call CONVERT_MDY
    call DOUBLE_CHECK
    call THE_END
  end
end /* end select */
/* say RDATE_VAL; exit 0 */
return RDATE_VAL
/**********************************************/
/*  E N D    O F   M A I N L I N E   C O D E  */
/**********************************************/

CONVERT_MDY:
if P1<1 | P1>12 then do
  say 'Invalid month passed to date routine'
  exit 12
end
if P2<1 | P2>31 then do
  say 'Invalid day passed to date routine'
  exit 12
end
if (P1=4 | P1=6 | P1=9 | P1=11) & P2>30 then do
  say 'Invalid day passed to date routine'
  exit 12
end
if P3<1900 | P3>2172 then do
  say 'Invalid year passed to date routine. Must be be 1900-2172'
  exit 12
end
BASE   = Substr(JULTBL,((P1-1)*3)+1,3)
if (P3//4=0 & P3<>1900 & P3<>2100) then LEAP= 1
  else LEAP = 0
if P1 > 2 then BASE = BASE+LEAP
JJJ = BASE + P2

MM   = P1
DD   = P2
YYYY = P3
return

CONVERT_NNNNN:
if P1<1 | P1>99712 then do
  say 'Invalid date passed to date routine. NNNNN must be 1-99712'
  exit 12
end
/* Determine YYYY and JJJ */
if P1>365 then P1=P1+1
YEARS_X4=(P1-1)%1461
JJJ=P1-YEARS_X4*1461
if P1 > 73415 then JJJ = JJJ +1
EXTRA_YEARS=(JJJ*3-3)%1096
JJJ=JJJ-(EXTRA_YEARS*1096+2)%3
YYYY=YEARS_X4*4+EXTRA_YEARS+1900
P1 = YYYY ; P2 = JJJ ;  call CONVERT_JDATE

CONVERT_JDATE:
if P1<1900 | P1>2172 then do
  say 'Invalid year passed to date routine. Must be be 1900-2172'
  exit 12
end
if P2<1 | P2>366 then do
  say 'Invalid Julian date passed to date routine'
  exit 12
end
if (P1//4=0 & P1<>1900 & P1<>2100) then LEAP= 1
  else LEAP = 0
ADJ1 = 0
ADJ2 = 0
Do MM = 1 to 11
   VAL1 = Substr(JULTBL,((MM-1)*3)+1,3)
   VAL2 = Substr(JULTBL,((MM-1)*3)+4,3)
   if MM >=2 then ADJ2 = LEAP
   if MM >=3 then ADJ1 = LEAP
   if P2 > VAL1+ADJ1 & P2 <= VAL2+ADJ2 then do
        DD = P2-VAL1-ADJ1
        MATCH = 'Y'
        leave
   end
end
if MATCH <> 'Y' then do
    MM = 12
    DD = P2-334-LEAP
end

YYYY = P1
JJJ  = P2
return

DOUBLE_CHECK:
if MM = 2 then do
   if DD > 28 & LEAP = 0 then do
     say 'Invalid day passed to date routine'
     exit 12
   end
   if DD > 29 & LEAP = 1 then do
     say 'Invalid day passed to date routine'
     exit 12
   end
end
if LEAP = 0 & JJJ > 365 then do
  say 'Invalid Julian date passed to date routine'
  exit 12
end
return

THE_END:
YR_1900 = YYYY-1900
NNNNN = (YR_1900*365) +(YR_1900+3)%4 + JJJ
if YYYY > 1900 then NNNNN = NNNNN-1
if YYYY > 2100 then NNNNN = NNNNN-1
INDEX   = NNNNN//7  /* index to DAY stem */
WEEKDAY =  DAY.INDEX

DD      = Right(DD,2,'0')
MM      = Right(MM,2,'0')
YYYY    = Strip(YYYY)
NNNNN   = Right(NNNNN,5,'0')
JJJ     = Right(JJJ,3,'0')

RDATE_VAL = MM||'/'||DD||'/'||YYYY||'.'||JJJ||' '||NNNNN||' '||WEEKDAY
return
