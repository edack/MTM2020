       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MIDTERM.
       AUTHOR.        EDWIN ACKERMAN.
       INSTALLATION.  MORONS LOSERS AND BIMBOS LP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE
               ASSIGN TO CLAIMFL.
           SELECT PRINT-FILE
               ASSIGN TO PRTLINE.
      *==========================================================*
       DATA DIVISION.
      *----------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------*
       FD  CLAIMS-FILE RECORDING MODE F.
       01  CLAIM-REC-CSV                   PIC X(80).
      *----------------------------------------------------------*
       FD  PRINT-FILE RECORDING MODE F.
       01  PRINT-RECORD.
      *    05 CC                           PIC X(01).
           05 PRINT-LINE                   PIC X(132).
      *----------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE            PIC X(132) VALUE SPACE.
      *----------------------------------------------------------*
       01  HEADING-LINES.
      *----------------------------------------------------------*
           05  HEADING-LINE-1.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(20) VALUE '  INSURANCE CLAIM RE'.
               10  FILLER  PIC X(20) VALUE 'PORT                '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(06) VALUE 'PAGE: '.
               10  HL1-PAGE-COUNT          PIC ZZ9.
               10  FILLER                  PIC X(03) VALUE SPACE.
      *----------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER  PIC X(20) VALUE 'INSURANCE           '.
               10  FILLER  PIC X(20) VALUE ' ACCOUNT FIRST     L'.
               10  FILLER  PIC X(20) VALUE 'AST             RENE'.
               10  FILLER  PIC X(20) VALUE 'WAL      POLICY     '.
               10  FILLER  PIC X(20) VALUE 'DED  DED         CLA'.
               10  FILLER  PIC X(20) VALUE 'IM      AMOUNT      '.
               10  FILLER  PIC X(12) VALUE '            '.
      *----------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER  PIC X(20) VALUE 'TYPE                '.
               10  FILLER  PIC X(20) VALUE ' NUMBER  NAME      N'.
               10  FILLER  PIC X(20) VALUE 'AME              DAT'.
               10  FILLER  PIC X(20) VALUE 'E        AMOUNT     '.
               10  FILLER  PIC X(20) VALUE 'AMT  PCT         AMO'.
               10  FILLER  PIC X(20) VALUE 'UNT      PAID       '.
               10  FILLER  PIC X(12) VALUE '            '.
      *----------------------------------------------------------*
           05  HEADING-LINE-4.
               10  FILLER  PIC X(132) VALUE ALL '-'.
      *----------------------------------------------------------*
       01 DETAIL-LINES.
      *----------------------------------------------------------*
           05  DETAIL-LINE-1.
               10  DET-POLICY-TYPE          PIC X(20).
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-NUM           PIC X(07).
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-F-NAME        PIC X(10).
               10  DL1-POLICY-L-NAME        PIC X(15).
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-YEAR          PIC 9(04).
               10  FILLER                   PIC X(01)  VALUE '/'.
               10  DL1-POLICY-MONTH         PIC X(02).
               10  FILLER                   PIC X(01)  VALUE '/'.
               10  DL1-POLICY-DAY           PIC X(02).
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-AMOUNT        PIC $$,$$$,$$9.99.
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-DED-PAID      PIC $$$9.
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-POLICY-COINS-PCT     PIC .99.
               10  FILLER                   PIC X(02)  VALUE '% '.
               10  DL1-CLAIM-MOUNT          PIC $$,$$$,$$9.99.
               10  FILLER                   PIC X(01)  VALUE ' '.
               10  DL1-CLAIM-AMT-PAID       PIC $$,$$$,$$9.99.

      *----------------------------------------------------------*
       COPY CLAIMREC.
      *----------------------------------------------------------*
      *----------------------------------------------------------*
       01  WS-COMPUTATION-FIELDS.
      *----------------------------------------------------------*
           05  WS-DEDUCTABLE-AMT            PIC S9(07)V99.
           05  WS-DEDUCTABLE-PCT            PIC V99     VALUE .20.
           05  WS-POLICY-COINSURANCE-NUM    PIC V9999.
           05  WS-CLAIM-AMT-PAID            PIC S9(07)V99.
           05  WS-CHECK-AMOUNT-NUM          PIC S9(07)V99.
      *----------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS-MISC.
      *----------------------------------------------------------*
           05  END-OF-FILE-SW               PIC X(01)   VALUE 'N'.
               88  END-OF-FILE                          VALUE 'Y'.
           05  VALID-CLAIM-SW               PIC X(01)   VALUE 'N'.
               88  VALID-CLAIM                          VALUE 'Y'.
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURRENT-DATE.
                   15  WS-CURRENT-YY        PIC 9(04).
                   15  WS-CURRENT-MM        PIC 9(02).
                   15  WS-CURRENT-DD        PIC 9(02).
               10  WS-CURRENT-TIME.
                   15  WS-CURRENT-HH        PIC 9(02).
                   15  WS-CURRENT-MM        PIC 9(02).
                   15  WS-CURRENT-SS        PIC 9(02).
                   15  WS-CURRENT-MS        PIC 9(02).
       COPY PRINTCTL.
      *==========================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *----------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-ACCT-FILE.
           PERFORM 2000-PROCESS-ACCT-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *----------------------------------------------------------*
       1000-OPEN-FILES.
      *----------------------------------------------------------*
           OPEN    INPUT  CLAIMS-FILE
                   OUTPUT PRINT-FILE.
      *----------------------------------------------------------*
       2000-PROCESS-ACCT-FILE.
      *----------------------------------------------------------*
           EVALUATE POLICY-TYPE
           WHEN 1
                MOVE 'EMPLOYER-PRIVATE'
                   TO DET-POLICY-TYPE
           WHEN 2
                MOVE 'STANDARD MEDICARE'
                   TO DET-POLICY-TYPE
           WHEN 3
                MOVE 'AFFORDABLE CARE ACT'
                   TO DET-POLICY-TYPE
           WHEN OTHER
                MOVE 'UNKNOWN' TO DET-POLICY-TYPE
           END-EVALUATE.
           MOVE INSURED-POLICY-NO          TO DL1-POLICY-NUM.
           MOVE INSURED-LAST-NAME          TO DL1-POLICY-L-NAME.
           MOVE INSURED-FIRST-NAME         TO DL1-POLICY-F-NAME.
           MOVE POLICY-YEAR                TO DL1-POLICY-YEAR.
           ADD 1                           TO DL1-POLICY-YEAR.
           MOVE POLICY-MONTH               TO DL1-POLICY-MONTH.
           MOVE POLICY-DAY                 TO DL1-POLICY-DAY.
           MOVE POLICY-AMOUNT              TO DL1-POLICY-AMOUNT.
           MOVE CLAIM-AMOUNT               TO DL1-CLAIM-MOUNT.
           DIVIDE POLICY-COINSURANCE       BY 100
               GIVING WS-POLICY-COINSURANCE-NUM.
           MOVE WS-POLICY-COINSURANCE-NUM  TO DL1-POLICY-COINS-PCT.
           MOVE POLICY-DEDUCTIBLE-PAID     TO DL1-POLICY-DED-PAID.
           PERFORM 2100-VALIDATE-CLAIM.
           IF  VALID-CLAIM
               MOVE WS-CLAIM-AMT-PAID      TO DL1-CLAIM-AMT-PAID
               MOVE CLAIM-AMOUNT           TO DL1-CLAIM-MOUNT
               MOVE DETAIL-LINE-1          TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
       2100-VALIDATE-CLAIM.
      *----------------------------------------------------------*
           MOVE 'N'                            TO VALID-CLAIM-SW.
           IF  CLAIM-AMOUNT <= POLICY-AMOUNT
               MOVE 'Y'                        TO VALID-CLAIM-SW
               COMPUTE WS-DEDUCTABLE-AMT       =
                   POLICY-AMOUNT               *
                   WS-DEDUCTABLE-PCT
               IF  WS-DEDUCTABLE-AMT       > POLICY-DEDUCTIBLE-PAID
                   MOVE  CLAIM-AMOUNT          TO WS-CLAIM-AMT-PAID
               ELSE
                   COMPUTE WS-CLAIM-AMT-PAID   =
                           CLAIM-AMOUNT        -
                           WS-DEDUCTABLE-AMT   -
                           (WS-POLICY-COINSURANCE-NUM  *
                            CLAIM-AMOUNT)
               END-IF
           END-IF.
      *----------------------------------------------------------*
       3000-CLOSE-FILES.
      *----------------------------------------------------------*
           CLOSE CLAIMS-FILE
                 PRINT-FILE.
      *----------------------------------------------------------*
       8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
           READ CLAIMS-FILE
               INTO CLAIM-RECORD-WS
                   AT END MOVE 'Y' TO END-OF-FILE-SW.
      *----------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
               PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *----------------------------------------------------------*
           MOVE PAGE-COUNT             TO HL1-PAGE-COUNT.
           MOVE HEADING-LINE-1         TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-2         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-3         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-4         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD  1                      TO PAGE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
           MOVE 5                      TO LINE-COUNT.
      *----------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                  TO PRINT-LINE.
      *----------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                  TO PRINT-LINE.
           ADD  1                      TO LINE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
