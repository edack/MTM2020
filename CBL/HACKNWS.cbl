      *===============================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HACKNEWS.
       AUTHOR.        EDWIN ACKERMAN.
       INSTALLATION.  IBM CLASS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE
               ASSIGN TO PRTLINE.
           SELECT HACKER-NEWS-FILE
               ASSIGN TO HACKNWS.
           SELECT SORT-FILE
               ASSIGN TO SORTFL
               FILE STATUS IS SR-STATUS.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*
       FD  PRINT-FILE RECORDING MODE F.
       01  PRINT-RECORD.
      *    05 CC                           PIC X(01).
           05 PRINT-LINE                   PIC X(132).
      *
       FD  HACKER-NEWS-FILE RECORDING MODE F.
       01  HACKER-NEWS-RECORD.
           05 FILLER                       PIC X(143).
       FD  SORT-FILE RECORDING MODE F.
       01  SORT-RECORD.
           05  SR-KEY                      PIC X(08).
           05  SR-TITLE                    PIC X(86).
           05  SR-VOTES                    PIC 9(04).
           05  SR-NUM-COMMENTS             PIC 9(04).
           05  SR-AUTHOR                   PIC X(15).
           05  SR-CREATED-TIME             PIC 99.99.
           05  SR-FILLER                   PIC X VALUE SPACE.
           05  SR-RANKING                  PIC ZZZ9.999999.
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE            PIC X(132) VALUE SPACE.
      *---------------------------------------------------------------*
       01  DETAIL-LINE.
      *---------------------------------------------------------------*
           05  DL-KEY                      PIC X(08).
           05  FILLER                      PIC X(05) VALUE SPACE.
           05  DL-TITLE                    PIC X(80).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-VOTES                    PIC X(04).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-CREATED-TIME             PIC 99.999.
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-HN-HH                    PIC X(02).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-HN-MM                    PIC X(03).
           05  FILLER                      PIC X(02) VALUE SPACE.
           05  DL-RANKING                  PIC Z9.999999.
      *---------------------------------------------------------------*
       01  HEADING-LINES.
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
               10  FILLER  PIC X(20) VALUE '         LISTING OF '.
               10  FILLER  PIC X(20) VALUE 'RECORDS WITH COBOL O'.
               10  FILLER  PIC X(20) VALUE 'R MAINFRAME IN THE T'.
               10  FILLER  PIC X(20) VALUE 'ITLE                '.
               10  FILLER  PIC X(20) VALUE '              PAGE: '.
               10  HL1-PAGE-COUNT          PIC ZZ9.
           05  HEADING-LINE-2.
               10  FILLER  PIC X(20) VALUE '         -----------'.
               10  FILLER  PIC X(20) VALUE '--------------------'.
               10  FILLER  PIC X(20) VALUE '--------------------'.
               10  FILLER  PIC X(20) VALUE '----                '.
               10  FILLER  PIC X(20) VALUE '                    '.
      *---------------------------------------------------------------*
       01  WS-HACKER-NEWS-RECORD.
      *---------------------------------------------------------------*
           05  HNR-KEY                     PIC X(08).
           05  HNR-TITLE                   PIC X(96).
           05  HNR-VOTES                   PIC 9(04).
           05  HNR-COMMENTS                PIC 9(04).
           05  HNR-AUTHOR                  PIC X(15).
           05  HNR-CREATED-DATE            PIC X(16).
           05  HNR-DATE                    PIC X(11).
           05  HNR-TIME                    PIC X(05) JUSTIFIED RIGHT.
           05  HNR-TIME-HH                 PIC 9(02).
           05  HNR-TIME-MM                 PIC 9(02).
      *---------------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS-MISC.
      *---------------------------------------------------------------*
           05  END-OF-FILE-SW              PIC X(01) VALUE 'N'.
               88  END-OF-FILE                       VALUE 'Y'.
           05  VALID-RECORD-SW             PIC X(01) VALUE 'Y'.
               88  VALID-RECORD                      VALUE 'Y'.
           05  COUNTER-1                   PIC 9(02) VALUE 0.
           05  COUNTER-2                   PIC 9(02) VALUE 0.
           05  SR-STATUS                   PIC X(02) VALUE '00'.
           05  WS-HN-TIME                  PIC 99V9999.
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURRENT-DATE.
                   15  WS-CURRENT-YY       PIC 9(04).
                   15  WS-CURRENT-MM       PIC 9(02).
                   15  WS-CURRENT-DD       PIC 9(02).
               10  WS-CURRENT-TIME.
                   15  WS-CURRENT-HH       PIC 9(02).
                   15  WS-CURRENT-MM       PIC 9(02).
                   15  WS-CURRENT-SS       PIC 9(02).
                   15  WS-CURRENT-MS       PIC 9(02).
           05 PRINTER-CONTROL-FIELDS.
               10  LINE-SPACEING           PIC 9(02) VALUE 1.
               10  LINE-COUNT              PIC 9(03) VALUE 999.
               10  LINES-ON-PAGE           PIC 9(02) VALUE 60.
               10  PAGE-COUNT              PIC 9(02) VALUE 1.
               10  TOP-OF-PAGE             PIC X(02) VALUE '1'.
               10  SINGLE-SPACE            PIC X(01) VALUE ' '.
               10  DOUBLE-SPACE            PIC X(01) VALUE '0'.
               10  TRIPLE-SPACE            PIC X(01) VALUE '-'.
               10  OVERPRINT               PIC X(01) VALUE '+'.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES-INITIALIZE.
           PERFORM 8000-READ-HACKER-NEWS-FILE.
           PERFORM 2000-PROCESS-HACKER-NEWS-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES-INITIALIZE.
      *---------------------------------------------------------------*
           OPEN INPUT  HACKER-NEWS-FILE
                OUTPUT SORT-FILE
                       PRINT-FILE.
      *---------------------------------------------------------------*
       2000-PROCESS-HACKER-NEWS-FILE.
      *---------------------------------------------------------------*
           MOVE 0 TO COUNTER-1, COUNTER-2.
           INSPECT FUNCTION UPPER-CASE(HNR-TITLE)
               TALLYING COUNTER-1 FOR ALL 'COBOL'.
           INSPECT FUNCTION UPPER-CASE(HNR-TITLE)
               TALLYING COUNTER-2 FOR ALL 'MAINFRAME'.
           IF  COUNTER-1  > ZERO OR
               COUNTER-2  > ZERO
               UNSTRING HNR-CREATED-DATE DELIMITED BY SPACE
                   INTO HNR-DATE
                        HNR-TIME
               INSPECT  HNR-TIME REPLACING ALL ' ' BY '0'
               UNSTRING HNR-TIME         DELIMITED BY ':'
                   INTO HNR-TIME-HH
                        HNR-TIME-MM
               PERFORM 2100-CALCULATE-RANKING
               MOVE HNR-KEY            TO SR-KEY
                                          DL-KEY
               MOVE HNR-TITLE          TO SR-TITLE
                                          DL-TITLE
               MOVE HNR-AUTHOR         TO SR-AUTHOR
               MOVE WS-HN-TIME         TO SR-CREATED-TIME
                                          DL-CREATED-TIME
               MOVE HNR-VOTES          TO SR-VOTES
                                          DL-VOTES
               MOVE HNR-COMMENTS       TO SR-NUM-COMMENTS
               MOVE DL-RANKING         TO SR-RANKING
               MOVE HNR-TIME-HH        TO DL-HN-HH
               MOVE HNR-TIME-MM        TO DL-HN-MM
               MOVE SPACE              TO SR-FILLER
               PERFORM 9200-WRITE-SORT-RECORD
               MOVE DETAIL-LINE        TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-HACKER-NEWS-FILE.
      *---------------------------------------------------------------*
       2100-CALCULATE-RANKING.
      *---------------------------------------------------------------*
           COMPUTE WS-HN-TIME = HNR-TIME-HH + (HNR-TIME-MM / 60)
           COMPUTE DL-RANKING = (HNR-VOTES - 1) ** .8
                              / (WS-HN-TIME + 2) ** 1.8.
      *---------------------------------------------------------------*
       3000-CLOSE-FILES.
      *---------------------------------------------------------------*
           CLOSE HACKER-NEWS-FILE
                 SORT-FILE
                 PRINT-FILE.
      *---------------------------------------------------------------*
       8000-READ-HACKER-NEWS-FILE.
      *---------------------------------------------------------------*
           READ HACKER-NEWS-FILE
               AT END MOVE 'Y' TO END-OF-FILE-SW
                      MOVE 'N' TO VALID-RECORD-SW.
           IF  VALID-RECORD
               PERFORM 8100-BREAKOUT-HACKER-RECORD.
      *---------------------------------------------------------------*
       8100-BREAKOUT-HACKER-RECORD.
      *---------------------------------------------------------------*
           INSPECT HACKER-NEWS-RECORD
               REPLACING ALL '"' BY '#'
               AFTER INITIAL '"'.
           INSPECT HACKER-NEWS-RECORD
               REPLACING ALL ',' BY ' '
               AFTER QUOTE BEFORE '#'.
           INSPECT HACKER-NEWS-RECORD
               REPLACING ALL '"' BY ' '
                         All '#' BY ' '.
           UNSTRING HACKER-NEWS-RECORD DELIMITED BY ','
                INTO HNR-KEY
                     HNR-TITLE
                     HNR-VOTES
                     HNR-COMMENTS
                     HNR-AUTHOR
                     HNR-CREATED-DATE.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
               PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE PAGE-COUNT           TO HL1-PAGE-COUNT.
           MOVE HEADING-LINE-1       TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                    TO LINE-SPACEING.
           MOVE HEADING-LINE-2       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD  1                    TO PAGE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
           MOVE 5                    TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                TO PRINT-LINE.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                TO PRINT-LINE.
           ADD  1                    TO LINE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
      *---------------------------------------------------------------*
       9200-WRITE-SORT-RECORD.
      *---------------------------------------------------------------*
           WRITE SORT-RECORD
               INVALID KEY PERFORM 9900-WRITE-FILE-ERROR.
           MOVE SPACE                TO SORT-RECORD.
      *---------------------------------------------------------------*
       9900-WRITE-FILE-ERROR.
      *---------------------------------------------------------------*
           MOVE SR-STATUS TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           DISPLAY SR-STATUS.

