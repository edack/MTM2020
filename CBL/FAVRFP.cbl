       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MUSICIAN-RFP     ASSIGN TO FAVRFP.
           SELECT REQ-FOR-PROPOSAL ASSIGN TO RFPFILE.
           SELECT RFP-RPT          ASSIGN TO PRTLINE.
       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------*
       FD  MUSICIAN-RFP
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS MUSICIAN-RFP-REC.
      *----------------------------------------------------------*
       01  MUSICIAN-RFP-REC.
           05  ARTIST-ACCT-NUM             PIC X(08).
           05  ARTIST-MUSICAL-GENRE        PIC X(06).
               88  ROCK                        VALUE 'ROCK'.
               88  JAZZ                        VALUE 'JAZZ'.
               88  FUSION                      VALUE 'FUSION'.
           05  MUSICIAN-NAME.
               10  MUSICIAN-LNAME          PIC X(15).
               10  MUSICIAN-FNAME          PIC X(15).
           05  INSTRUMENT-TYPE             PIC X(06).
               88  KEYBOARD                    VALUE 'KEYS  '.
               88  VOCALS                      VALUE 'VOCALS'.
               88  GUITAR                      VALUE 'GUITAR'.
               88  BASS                        VALUE 'BASS  '.
               88  DRUMS                       VALUE 'DRUMS '.
               88  PERCUSSION                  VALUE 'PERC  '.
           05  INSTRUMENT-QUALITY          PIC X(01).
               88  USED-FLAG                   VALUE 'U'.
               88  NEW-FLAG                    VALUE 'N'.
               88  PREMIUM-FLAG                VALUE 'P'.
           05  MAX-MUSICIAN-BUDGET-AMT     PIC 9(05)V99.
           05  SHIP-TO-FLAG                PIC X(03).
               88  IN-COUNTRY                  VALUE 'IN '.
               88  OUT-COUNTRY                 VALUE 'OUT'.
           05  FILLER                      PIC X(19).
      *----------------------------------------------------------*
       FD  REQ-FOR-PROPOSAL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
      *----------------------------------------------------------*
       01  RFP-RECORD.
           05  RFP-ARTIST-ACCT-NO          PIC X(08).
           05  RFP-MUSICIAN-NAME.
               10  RFP-MUSICIAN-LNAME      PIC X(15).
               10  RFP-MUSICIAN-FNAME      PIC X(15).
           05  RFP-INSTRUMENT              PIC 9(06).
           05  RFP-INSTRUMENT-QUALITY      PIC X(01).
           05  RFP-SHIP-TO-FLAG            PIC X(03).
           05  RFP-COST-PER-INSTRUMENT     PIC S9(07)V99.
           05  RFP-ADDITIONAL-COSTS.
               10  RFP-SHIPPING-COST       PIC S9(04)V99.
               10  RFP-TAX-AMT             PIC S9(03)V99.
           05  FILLER                      PIC X(01) VALUE SPACE.
      *----------------------------------------------------------*
       FD  RFP-RPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PRINT-RECORD.
      *----------------------------------------------------------*
       01  PRINT-RECORD.
      *    05  CARRAGE-CONTROL             PIC X(01).
           05  PRINT-LINE                  PIC X(132).
      *----------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE            PIC X(132) VALUE SPACE.
      *----------------------------------------------------------*
           05  WS-RFP-RECORD.
               10  WS-ARTIST-ACCT-NO       PIC X(08).
               10  WS-MUSICIAN-NAME.
                   15  WS-MUSICIAN-LNAME   PIC X(15).
                   15  WS-MUSICIAN-FNAME   PIC X(15).
               10  WS-INSTRUMENT           PIC X(06).
               10  WS-INSTRUMENT-QUALITY   PIC X(01).
               10  WS-SHIP-TO-FLAG         PIC X(03).
               10  WS-COST-PER-INSTRUMENT  PIC S9(07)V99.
               10  WS-ADDITIONAL-COSTS.
                   15  WS-SHIPPING-COST    PIC S9(04)V99.
                   15  WS-TAX-RATE         PIC S9(03)V99.
               10  FILLER                  PIC X(01) VALUE SPACE.
      *----------------------------------------------------------*
           05  REPORT-DETAIL-LINE.
               10  RDL-ARTIST-ACCT-NO      PIC X(08).
               10  RDL-MUSICIAN-NAME.
                   15  RDL-MUSICIAN-LNAME  PIC X(15).
                   15  RDL-MUSICIAN-FNAME  PIC X(15).
               10  RDL-INSTRUMENT          PIC X(06).
               10  RDL-INSTRUMENT-QUALITY  PIC X(01).
               10  RDL-SHIP-TO-FLAG        PIC X(03).
               10  FILLER                  PIC X(03) VALUE SPACE.
               10  RDL-COST-PER-INSTRUMENT PIC 9(07).99.
               10  FILLER                  PIC X(03) VALUE SPACE.
               10  RDL-ADDITIONAL-COSTS.
                   15  RDL-SHIPPING-COST   PIC 9(04).99.
                   15  FILLER              PIC X(03) VALUE SPACE.
                   15  RDL-TAX-AMT         PIC 9(04).99.
               10  FILLER                  PIC X(01) VALUE SPACE.
      *----------------------------------------------------------*
          05  HEADING-LINE-1.
               10  FILLER  PIC X(06) VALUE 'DATE: '.
               10  HDR-DAY PIC X(02).
               10  FILLER  PIC X(01) VALUE '/'.
               10  HDR-MO  PIC X(02).
               10  FILLER  PIC X(01) VALUE '/'.
               10  HDR-YR  PIC X(04).
               10  FILLER  PIC X(08) VALUE SPACES.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(20) VALUE 'MUSICIAN RFP LISTING'.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(08) VALUE SPACES.
               10  FILLER  PIC X(10) VALUE 'PAGE NUM: '.
               10  H1-PAGE-NUM PIC 999.
      *----------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER  PIC X(20) VALUE 'ARTIST              '.
               10  FILLER  PIC X(20) VALUE '            NUM  MUS'.
               10  FILLER  PIC X(20) VALUE 'IC         CD       '.
               10  FILLER  PIC X(20) VALUE 'TAX     SHIP     TOT'.
               10  FILLER  PIC X(20) VALUE 'AL    STILL         '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *----------------------------------------------------------*
            05  HEADING-LINE-3.
               10  FILLER  PIC X(20) VALUE 'NAME                '.
               10  FILLER  PIC X(20) VALUE '            MUS  GEN'.
               10  FILLER  PIC X(20) VALUE 'RE         COST     '.
               10  FILLER  PIC X(20) VALUE 'AMT     COST     COS'.
               10  FILLER  PIC X(20) VALUE 'T     TOGETHER      '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *----------------------------------------------------------*
            05  HEADING-LINE-4.
               10  FILLER  PIC X(20) VALUE '--------------------'.
               10  FILLER  PIC X(20) VALUE '----------  ---  ---'.
               10  FILLER  PIC X(20) VALUE '--         ----     '.
               10  FILLER  PIC X(20) VALUE '---     ----     ---'.
               10  FILLER  PIC X(20) VALUE '-     --------      '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(12) VALUE '            '.
      *----------------------------------------------------------*
           05  TRAILER-1.
               10  FILLER  PIC X(20) VALUE '** TOTAL RECORDS PRO'.
               10  FILLER  PIC X(08) VALUE 'CESSED :'.
               10  TL1-RECORD-COUNT        PIC ZZ9.
               10  FILLER                  PIC X(101) VALUE SPACE.
      *----------------------------------------------------------*
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-MILLISECONDS PIC 9(02).
      *----------------------------------------------------------*
       01  WS-NUMERIC-FIELDS.
           05  WS-INSTRUMENT-COST-NUM      PIC 9(07)V99.
           05  WS-QUALITY-NUM              PIC 9(01)V99.
           05  WS-TAX-AMT-NUM              PIC 9(03)V99.
           05  WS-SHIPPING-RATE-NUM        PIC 9(01)V99.
           05  WS-SHIPPING-COST-NUM        PIC 9(04)V99.
           05  WS-RECORD-COUNT             PIC 9(03) VALUE ZERO.
      *----------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS.
           05  END-OF-FILE-SW              PIC X VALUE 'N'.
               88  END-OF-FILE                   VALUE 'Y'.
      *----------------------------------------------------------*
       01 PRINTER-CONTROL-FIELDS.
           05  LINE-SPACEING               PIC 9(02) VALUE 1.
           05  LINE-COUNT                  PIC 9(03) VALUE 999.
           05  LINES-ON-PAGE               PIC 9(02) VALUE 60.
           05  PAGE-COUNT                  PIC 9(02) VALUE 1.
           05  TOP-OF-PAGE                 PIC X(02) VALUE '1'.
           05  SINGLE-SPACE                PIC X(01) VALUE ' '.
           05  DOUBLE-SPACE                PIC X(01) VALUE '0'.
           05  TRIPLE-SPACE                PIC X(01) VALUE '-'.
           05  OVERPRINT                   PIC X(01) VALUE '+'.
      *----------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------*
       0000-PROCESS-FAVORITE-GROUPS.
      *----------------------------------------------------------*
           PERFORM 1000-INITIALIZATION.
           PERFORM 8000-READ-RFP-FILE.
           PERFORM 2000-PROCESS-FAV-GRP-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-PRINT-TRAILER-LINES.
           PERFORM 4000-CLOSING.
           GOBACK.
      *----------------------------------------------------------*
       1000-INITIALIZATION.
      *----------------------------------------------------------*
           OPEN INPUT MUSICIAN-RFP
                OUTPUT REQ-FOR-PROPOSAL
                       RFP-RPT.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR        TO HDR-YR.
           MOVE WS-CURRENT-MONTH       TO HDR-MO.
           MOVE WS-CURRENT-DAY         TO HDR-DAY.
      *----------------------------------------------------------*
       2000-PROCESS-FAV-GRP-FILE.
      *----------------------------------------------------------*
           MOVE ARTIST-ACCT-NUM        TO RDL-ARTIST-ACCT-NO
                                          RFP-ARTIST-ACCT-NO.
           MOVE MUSICIAN-NAME          TO RDL-MUSICIAN-NAME
                                          RFP-MUSICIAN-NAME.
           MOVE INSTRUMENT-TYPE        TO RDL-INSTRUMENT
                                          RFP-INSTRUMENT.
           MOVE INSTRUMENT-QUALITY     TO RDL-INSTRUMENT-QUALITY
                                          RFP-INSTRUMENT-QUALITY.
           MOVE SHIP-TO-FLAG           TO RDL-SHIP-TO-FLAG
                                          RFP-SHIP-TO-FLAG.
           PERFORM 2100-COMPUTE-INSTRUMENT-COSTS.
           MOVE WS-SHIPPING-COST-NUM   TO RDL-SHIPPING-COST
                                          RFP-SHIPPING-COST.
           MOVE WS-TAX-AMT-NUM         TO RDL-TAX-AMT
                                          RFP-TAX-AMT.
           ADD 1                       TO WS-RECORD-COUNT.
           MOVE REPORT-DETAIL-LINE     TO NEXT-REPORT-LINE.
           PERFORM  9000-PRINT-REPORT-LINE.
           PERFORM  9100-WRITE-RFP-RECORD.
           PERFORM  8000-READ-RFP-FILE.
      *----------------------------------------------------------*
       2100-COMPUTE-INSTRUMENT-COSTS.
      *----------------------------------------------------------*
           EVALUATE TRUE
               WHEN KEYBOARD
                   MOVE  3017.89         TO  WS-INSTRUMENT-COST-NUM
               WHEN VOCALS
                   MOVE   599.05         TO  WS-INSTRUMENT-COST-NUM
               WHEN GUITAR
                   MOVE  2648.99         TO  WS-INSTRUMENT-COST-NUM
               WHEN BASS
                   MOVE  1875.10         TO  WS-INSTRUMENT-COST-NUM
               WHEN DRUMS
                   MOVE  3087.22         TO  WS-INSTRUMENT-COST-NUM
               WHEN PERCUSSION
                   MOVE   799.99         TO  WS-INSTRUMENT-COST-NUM.
           EVALUATE TRUE
               WHEN USED-FLAG
                 MOVE  -0.20             TO  WS-QUALITY-NUM
               WHEN NEW-FLAG
                 MOVE  0                 TO  WS-QUALITY-NUM
               WHEN PREMIUM-FLAG
                 MOVE   0.20             TO  WS-QUALITY-NUM.
           IF  IN-COUNTRY
               MOVE  0.10                TO  WS-SHIPPING-RATE-NUM
           ELSE
               MOVE  0.20                TO  WS-SHIPPING-RATE-NUM.
           COMPUTE WS-SHIPPING-COST-NUM = WS-SHIPPING-RATE-NUM
                       * WS-INSTRUMENT-COST-NUM.
           COMPUTE WS-TAX-AMT-NUM       = WS-INSTRUMENT-COST-NUM
                       *  0.08.
           COMPUTE WS-COST-PER-INSTRUMENT = WS-INSTRUMENT-COST-NUM
                       + (WS-INSTRUMENT-COST-NUM * WS-QUALITY-NUM)
                       + WS-TAX-AMT-NUM.
           MOVE WS-COST-PER-INSTRUMENT     TO RDL-COST-PER-INSTRUMENT.
      *----------------------------------------------------------*
       3000-PRINT-TRAILER-LINES.
      *----------------------------------------------------------*
           MOVE WS-RECORD-COUNT        TO TL1-RECORD-COUNT.
           MOVE TRAILER-1              TO NEXT-REPORT-LINE.
           MOVE 2                      TO LINE-SPACEING.
           PERFORM 9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
       4000-CLOSING.
      *----------------------------------------------------------*
           CLOSE   MUSICIAN-RFP
                   REQ-FOR-PROPOSAL
                   RFP-RPT.
      *----------------------------------------------------------*
       8000-READ-RFP-FILE.
      *----------------------------------------------------------*
           READ MUSICIAN-RFP
               AT END MOVE 'Y' TO END-OF-FILE-SW.
      *----------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
               PERFORM 9010-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE TO PRINT-LINE.
           PERFORM 9012-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
       9010-PRINT-HEADING-LINES.
      *----------------------------------------------------------*
           MOVE PAGE-COUNT             TO H1-PAGE-NUM.
           MOVE HEADING-LINE-1         TO PRINT-LINE.
           PERFORM 9011-WRITE-TOP-OF-PAGE.
           MOVE 2                      TO LINE-SPACEING.
           MOVE HEADING-LINE-2         TO PRINT-LINE.
           PERFORM 9012-WRITE-PRINT-LINE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-3         TO PRINT-LINE.
           PERFORM 9012-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4         TO PRINT-LINE.
           PERFORM 9012-WRITE-PRINT-LINE.
           ADD  1                      TO PAGE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
           MOVE 5                      TO LINE-COUNT.
      *----------------------------------------------------------*
       9011-WRITE-TOP-OF-PAGE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                  TO PRINT-LINE.
      *----------------------------------------------------------*
       9012-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                  TO PRINT-LINE.
           ADD  1                      TO LINE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
      *---------------------------------------------------------*
       9100-WRITE-RFP-RECORD.
      *----------------------------------------------------------*
           WRITE RFP-RECORD.
