/**************************** REXX *********************************/
/* This exec illustrates the use of "EXECIO 0 ..." to open, empty, */
/* or close a file. It writes records to file outdd,               */
/*******************************************************************/
"FREE FI(outdd)"
"ALLOC FI(outdd) DA('Z00070.OUTPUT(CUST16)') SHR REUSE"
eofflag = 2                 /* Return code to indicate end-of-file */
return_code = 0             /* Initialize return code              */
in_ctr = 0                  /* Initialize # of lines read          */
out_ctr = 0                 /* Initialize # of lines written       */
inv_card = 0
cc_digits = 0

"EXECIO 0 DISKW outdd (OPEN FINIS"  /* Empty the outdd file        */
SAY 'File outdd is now empty.'
/* -----------------------------------------------------------------------*/
DO WHILE (out_ctr < 500)
    card_num = ''
    DO indx = 1 to 15
        cc_digits = random(1,9)
        temp = card_num||cc_digits
        card_num = temp
    END
    cc_digits = temp||'0'
    call INSPECT
    temp = reverse(cc_digits)
    cc_digits = substr(temp,1,15)||result
    line.1 = cc_digits
    SAY "cc_digits " cc_digits
    "EXECIO 1 DISKW outdd (STEM line."    /* Write it to out                 */
    out_ctr = out_ctr + 1                 /* Increment output lines          */
END
IF out_ctr > 0 THEN                       /* Were any lines written to outdd?*/
    DO                                    /* Yes.  So outdd is now open      */
        "EXECIO 0 DISKW outdd (FINIS"     /* Closes the open file, outdd     */
        SAY 'File outdd now contains ' out_ctr' lines.'
        SAY 'invalid card count ' inv_card
    END
ELSE                                      /* Else no new lines written       */
    DO                                    /* Erase old records from the file */
       "EXECIO 0 DISKW outdd (OPEN FINIS" /* Empty the outdd file            */
        SAY 'File outdd is now empty.'
    END
/*                                 */
"FREE FI(outdd)"
EXIT

/* -----------------------------------------------------------------------*/
INSPECT:
say 'inspecting' cc_digits
cc_digits = reverse(cc_digits)
checksum = 0
odd_check = 0
even_check = 0
even_hold  = 0
DO indx = 2 to 16
    IF (indx//2 > 0) then
        odd_check = odd_check + substr(cc_digits,indx,1)
    ELSE
        DO
            even_hold = substr(cc_digits,indx,1) * 2
            IF even_hold > 9 THEN
                even_hold = even_hold - 9
            even_check = even_check + even_hold
        END
    checksum = odd_check + even_check
END
check_digit = (checksum * 9) // 10
say "check digit " check_digit
RETURN check_digit
