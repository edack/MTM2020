      *---------------------------------------------------------------*
       01  UHR-RECORD-IN.
      *---------------------------------------------------------------*
           05 UHR-DATE.
               10 UHR-YEAR                 PIC X(04).
               10 UHR-MONTH                PIC X(02).
               10 UHR-DAY                  PIC X(02).
           05 UHR-STATE                    PIC X(02).
           05 UHR-CASE-POSITIVE            PIC 9(08).
           05 UHR-CASE-NEGATIVE            PIC 9(08).
           05 UHR-CASE-PENDING             PIC 9(08).
           05 UHR-HOSPITAL-CURR            PIC 9(07).
           05 UHR-HOSPITAL-TOT             PIC 9(07).
           05 UHR-ICU-CURR                 PIC 9(07).
           05 UHR-ICU-TOT                  PIC 9(07).
           05 UHR-VENT-CURR                PIC 9(07).
           05 UHR-VENT-TOT                 PIC 9(07).
           05 UHR-RECOVERED                PIC 9(07).
           05 UHR-DATE-CHECKED             PIC X(20).
           05 UHR-DEATH                    PIC 9(06).
           05 UHR-HOSPTALIZED              PIC 9(07).
           05 UHR-TOT-TESTS                PIC 9(09).
           05 UHR-LAST-MODIFIED            PIC X(20).
           05 UHR-TOTAL                    PIC 9(07).
           05 UHR-POS-NEG                  PIC 9(07).
           05 UHR-DEATH-INCREASE           PIC 9(07).
           05 UHR-HOSPITAL-INCREASE        PIC 9(06).
           05 UHR-NEGATIVE-INCREASE        PIC 9(06).
           05 UHR-POSITIVE-INCREASE        PIC 9(06).
           05 UHR-TOT-TEST-INCREASE        PIC 9(06).
           05 UHR-HASH                     PIC X(35).
