      *==========================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBL0106A.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO PRTLINE.
           SELECT ACCT-FILE  ASSIGN TO ACCTREC.
      *==========================================================*
       DATA DIVISION.
      *----------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------*
       FD  PRINT-FILE RECORDING MODE F.
       01  PRINT-RECORD.
      *    05 CC                     PIC X(01).
           05 PRINT-LINE             PIC X(132).
      *
       FD  ACCT-FILE RECORDING MODE F.
       COPY ACCTREC.
      *----------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE      PIC X(132) VALUE SPACE.
      *----------------------------------------------------------*
           05  STATE-COUNT-LINE.
               10 SCL-STATE-NAME     PIC X(19) VALUE SPACE.
               10 FILLER             PIC X(05) VALUE '   = '.
               10 SCL-STATE-COUNT    PIC ZZ9.
               10 FILLER             PIC X(59) VALUE SPACES.
      *----------------------------------------------------------*
           05  OVERLIMIT-STATUS-COUNT.
               10 FILLER             PIC X(24) VALUE
                  'Account Overlimit Cnt = '.
               10 OSC-COUNT          PIC ZZ9.
               10 FILLER             PIC X(66) VALUE SPACE.
      *----------------------------------------------------------*
           05  OVERLIMIT-STATUS-LINE.
               10 FILLER             PIC X(20) VALUE SPACE.
               10 OSL-MESSAGE        PIC X(30) VALUE SPACE.
               10 FILLER             PIC X(50) VALUE SPACE.
      *----------------------------------------------------------*
           05  WS-PRINT-RECORD.
               10  WS-ACCT-NUM-O     PIC X(8).
               10  FILLER            PIC X(02) VALUE SPACES.
               10  WS-LAST-NAME-O    PIC X(20).
               10  FILLER            PIC X(02) VALUE SPACES.
               10  WS-ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
               10  FILLER            PIC X(03) VALUE SPACES.
               10  WS-ACCT-BALANCE-O PIC $$,$$$,$$9.99.
               10  FILLER            PIC X(02) VALUE SPACES.
      *----------------------------------------------------------*
           05  OVERLIMIT-DETAIL.
               10  OD-ACCT-NUM       PIC X(8).
               10  FILLER            PIC X(02) VALUE SPACES.
               10  OD-LAST-NAME      PIC X(20).
               10  FILLER            PIC X(02) VALUE SPACES.
               10  OD-FIRST-NAME     PIC X(14).
               10  FILLER            PIC X(02) VALUE SPACES.
               10  OD-OVER-AMT       PIC $$,$$$,$$9.99.
               10  FILLER            PIC X(23) VALUE SPACES.
      *----------------------------------------------------------*
       01  HEADING-LINES.
      *----------------------------------------------------------*
           05  HEADING-LINE-1.
               10  FILLER      PIC X(20) VALUE 'Financial Report for'.
               10  FILLER      PIC X(38) VALUE SPACES.
               10  FILLER      PIC X(14) VALUE 'USA Presidents'.
               10  FILLER      PIC X(38) VALUE SPACES.
               10  FILLER      PIC X(10) VALUE 'PAGE NUM'.
               10  H1-PAGE-NUM PIC 999.
      *----------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER         PIC X(05) VALUE 'Year '.
               10  HDR-YR         PIC 9(04).
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(06) VALUE 'Month '.
               10  HDR-MO         PIC X(02).
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(04) VALUE 'Day '.
               10  HDR-DAY        PIC X(02).
               10  FILLER         PIC X(56) VALUE SPACES.
      *----------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER         PIC X(08) VALUE 'Account '.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE 'Last Name '.
               10  FILLER         PIC X(15) VALUE SPACES.
               10  FILLER         PIC X(06) VALUE 'Limit '.
               10  FILLER         PIC X(07) VALUE SPACES.
               10  FILLER         PIC X(08) VALUE 'Balance '.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
           05  HEADING-LINE-4.
               10  FILLER         PIC X(08) VALUE '--------'.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE '----------'.
               10  FILLER         PIC X(15) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE '----------'.
               10  FILLER         PIC X(03) VALUE SPACES.
               10  FILLER         PIC X(13) VALUE '-------------'.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
       01  TRAILER-LINES.
      *----------------------------------------------------------*
           05  TRAILER-1.
               10  FILLER         PIC X(31) VALUE SPACES.
               10  FILLER         PIC X(14) VALUE '--------------'.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(14) VALUE '--------------'.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
           05  TRAILER-2.
               10  FILLER         PIC X(22) VALUE SPACES.
               10  FILLER         PIC X(08) VALUE 'Totals ='.
               10  FILLER         PIC X(01) VALUE SPACES.
               10  TLIMIT-O       PIC $$$,$$$,$$9.99.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  TBALANCE-O     PIC $$$,$$$,$$9.99.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
           05  STATE-TRAILER-1.
               10  FILLER  PIC X(20) VALUE '         Listing of '.
               10  FILLER  PIC X(20) VALUE 'States and Count of '.
               10  FILLER  PIC X(20) VALUE 'Presidents          '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(20) VALUE '                    '.
      *----------------------------------------------------------*
           05  TRAILER-LINE-1.
               10  FILLER         PIC X(08) VALUE 'Account '.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE 'Last Name '.
               10  FILLER         PIC X(12) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE 'First Name'.
               10  FILLER         PIC X(06) VALUE SPACES.
               10  FILLER         PIC X(13) VALUE 'Amt Overlimit'.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
           05  TRAILER-LINE-2.
               10  FILLER         PIC X(08) VALUE '--------'.
               10  FILLER         PIC X(02) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE '----------'.
               10  FILLER         PIC X(12) VALUE SPACES.
               10  FILLER         PIC X(10) VALUE '----------'.
               10  FILLER         PIC X(06) VALUE SPACES.
               10  FILLER         PIC X(13) VALUE '-------------'.
               10  FILLER         PIC X(40) VALUE SPACES.
      *----------------------------------------------------------*
       COPY PRINTCTL.
      *----------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS.
           05  END-OF-FILE-SW              PIC X VALUE 'N'.
               88  END-OF-FILE                   VALUE 'Y'.
           05  OVERLIMIT-COUNT             PIC 999 VALUE 0.
           05  INDEX-1                     PIC 999 VALUE 1.
           05  NO-OVERLIMIT-STATUS         PIC X(32) VALUE
               '  ***  NO ACCTS OVERLIMIT  ***  '.
           05  YES-OVERLIMIT-STATUS        PIC X(32) VALUE
               '   ***  ACCTS OVERLIMIT   ***   '.
           05 TLIMIT                       PIC S9(9)V99 COMP-3 VALUE 0.
           05 TBALANCE                     PIC S9(9)V99 COMP-3 VALUE 0.

           05  STATE-COUNT-TABLE   OCCURS 45 TIMES
                   INDEXED BY STATE-INDEX.
               10  STATE-NAME              PIC X(15) VALUE SPACE .
               10  STATE-COUNT             PIC 9(02) VALUE ZERO.
           05  OVERLIMIT OCCURS 20 TIMES.
               10  OL-ACCT-NO              PIC X(8).
               10  OL-ACCT-LIMIT           PIC S9(7)V99 COMP-3.
               10  OL-ACCT-BALANCE         PIC S9(7)V99 COMP-3.
               10  OL-LASTNAME             PIC X(20).
               10  OL-FIRSTNAME            PIC X(15).
      *==========================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *----------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-ACCT-FILE.
           PERFORM 2000-PROCESS-ACCT-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-PRINT-TRAILER-LINES.
           PERFORM 4000-CLOSE-FILES.
           GOBACK.
      *----------------------------------------------------------*
       1000-OPEN-FILES.
      *----------------------------------------------------------*
           OPEN INPUT  ACCT-FILE
                OUTPUT PRINT-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR  TO HDR-YR.
           MOVE WS-CURRENT-MONTH TO HDR-MO.
           MOVE WS-CURRENT-DAY   TO HDR-DAY.
      *----------------------------------------------------------*
       2000-PROCESS-ACCT-FILE.
      *----------------------------------------------------------*
           MOVE ACCT-NO          TO WS-ACCT-NUM-O.
           MOVE ACCT-LIMIT       TO WS-ACCT-LIMIT-O.
           MOVE ACCT-BALANCE     TO WS-ACCT-BALANCE-O.
           MOVE LAST-NAME        TO WS-LAST-NAME-O.
           COMPUTE TLIMIT   = TLIMIT   + ACCT-LIMIT.
           COMPUTE TBALANCE = TBALANCE + ACCT-BALANCE.
           IF ACCT-LIMIT < ACCT-BALANCE THEN
               MOVE ACCT-NO      TO OL-ACCT-NO(INDEX-1)
               MOVE ACCT-LIMIT   TO OL-ACCT-LIMIT(INDEX-1)
               MOVE ACCT-BALANCE TO OL-ACCT-BALANCE(INDEX-1)
               MOVE LAST-NAME    TO OL-LASTNAME(INDEX-1)
               MOVE FIRST-NAME   TO OL-FIRSTNAME(INDEX-1)
               ADD 1 TO INDEX-1
               ADD 1 TO OVERLIMIT-COUNT.
           PERFORM 2100-ACCUMULATE-STATE-TOTALS.
           MOVE WS-PRINT-RECORD  TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
       2100-ACCUMULATE-STATE-TOTALS.
      *----------------------------------------------------------*
           SET STATE-INDEX TO 1.
           SEARCH STATE-COUNT-TABLE
               AT END
                   PERFORM 9900-TABLE-ERROR
               WHEN STATE-NAME(STATE-INDEX) = USA-STATE
                   ADD 1            TO STATE-COUNT(STATE-INDEX)
               WHEN STATE-NAME(STATE-INDEX) = SPACE
                   MOVE USA-STATE   TO STATE-NAME(STATE-INDEX)
                   ADD 1            TO STATE-COUNT(STATE-INDEX).
      *----------------------------------------------------------*
       3000-PRINT-TRAILER-LINES.
      *----------------------------------------------------------*
           MOVE TLIMIT   TO TLIMIT-O.
           MOVE TBALANCE TO TBALANCE-O.
           MOVE TRAILER-1                  TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           MOVE TRAILER-2                  TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           MOVE 1                          TO LINE-COUNT.
           MOVE STATE-TRAILER-1            TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                          TO LINE-SPACEING.
           PERFORM  3100-PRINT-STATE-TOTALS
               VARYING STATE-INDEX FROM 1 BY 1
                   UNTIL STATE-COUNT(STATE-INDEX) = ZERO.
           IF OVERLIMIT-COUNT = 0
               MOVE NO-OVERLIMIT-STATUS    TO OSL-MESSAGE
               MOVE OVERLIMIT-STATUS-LINE  TO NEXT-REPORT-LINE
               MOVE 3                      TO LINE-SPACEING
               PERFORM 9000-PRINT-REPORT-LINE
           ELSE
               MOVE OVERLIMIT-COUNT        TO OSC-COUNT
               MOVE OVERLIMIT-STATUS-COUNT TO NEXT-REPORT-LINE
               MOVE 3                      TO LINE-SPACEING
               PERFORM 9000-PRINT-REPORT-LINE
               MOVE 2                      TO LINE-SPACEING
               MOVE YES-OVERLIMIT-STATUS   TO OSL-MESSAGE
               MOVE OVERLIMIT-STATUS-LINE  TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE
               MOVE 2                      TO LINE-SPACEING
               MOVE TRAILER-LINE-1         TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE
               MOVE 1                      TO LINE-SPACEING
               MOVE TRAILER-LINE-2         TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE
               MOVE 2                      TO LINE-SPACEING
               PERFORM 3200-PRINT-OVERLIMIT-DETAIL
                   VARYING INDEX-1 FROM 1 BY 1
                   UNTIL INDEX-1 > OVERLIMIT-COUNT.
      *----------------------------------------------------------*
       3100-PRINT-STATE-TOTALS.
      *----------------------------------------------------------*
           MOVE  STATE-NAME(STATE-INDEX)  TO  SCL-STATE-NAME.
           MOVE  STATE-COUNT(STATE-INDEX) TO  SCL-STATE-COUNT.
           MOVE  STATE-COUNT-LINE         TO  NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
       3200-PRINT-OVERLIMIT-DETAIL.
      *----------------------------------------------------------*
           SUBTRACT OL-ACCT-LIMIT(INDEX-1)
               FROM OL-ACCT-BALANCE(INDEX-1)
               GIVING OD-OVER-AMT.
           MOVE OL-ACCT-NO(INDEX-1)       TO OD-ACCT-NUM.
           MOVE OL-LASTNAME(INDEX-1)      TO OD-LAST-NAME.
           MOVE OL-FIRSTNAME(INDEX-1)     TO OD-FIRST-NAME.
           MOVE OVERLIMIT-DETAIL       TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
       4000-CLOSE-FILES.
      *----------------------------------------------------------*
           CLOSE ACCT-FILE
                 PRINT-FILE.
      *----------------------------------------------------------*
       8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
           READ ACCT-FILE
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
           MOVE PAGE-COUNT           TO H1-PAGE-NUM.
           MOVE HEADING-LINE-1       TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                    TO LINE-SPACEING.
           MOVE HEADING-LINE-2       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                    TO LINE-SPACEING.
           MOVE HEADING-LINE-3       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD  1                    TO PAGE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
           MOVE 5                    TO LINE-COUNT.
      *----------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                TO PRINT-LINE.
      *----------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                TO PRINT-LINE.
           ADD  1                    TO LINE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
      *---------------------------------------------------------*
       9900-TABLE-ERROR.
      *---------------------------------------------------------*
