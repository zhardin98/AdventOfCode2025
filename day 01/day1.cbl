       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECRET-ENTRANCE.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 1 2025.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2025 DAY 1 PROBLEM                     *
      * LINK: https://adventofcode.com/2025/day/1                   *
      *************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    INPUT FILE
           SELECT INPUT-FILE ASSIGN TO 'INFILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 4 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05 IN-DIRECTION                      PIC X(1).
              88 DIR-LEFT                                  VALUE 'L'.
              88 DIR-RIGHT                                 VALUE 'L'.
           05 IN-CLICKS                         PIC X(3).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                             PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       01  WS-EOF                               PIC X(1).
           88 EOF                                          VALUE 'Y'.
           88 NOT-EOF                                      VALUE 'N'.

       01  WS-DIAL-POS                          PIC S9(3)  VALUE 50.

       01  WS-CLICK-DIGIT-LENGTH                PIC 9(1).
       01  WS-CLICK-AMT                         PIC 9(3).
       
       01  OUT-TIMES-AT-ZERO                    PIC 9(7)   VALUE 0.

       01  WS-END                               PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE      THRU 1000-EXIT
           PERFORM 2000-ROTATE-DIAL    THRU 2000-EXIT
               UNTIL EOF
           PERFORM 9000-CLOSE-FILE     THRU 9000-EXIT
           DISPLAY OUT-TIMES-AT-ZERO ' TIMES AT ZERO'
           .
       0000-EXIT.
           GOBACK.

      ****************************************************************
      * OPEN FILE                                                    *
      ****************************************************************
       1000-OPEN-FILE.

           OPEN INPUT INPUT-FILE
           MOVE SPACE TO WS-EOF
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * ROTATE DIAL AMOUNT OF CLICKS NECCESARY                       *
      ****************************************************************
       2000-ROTATE-DIAL.

      *    READ INFILE THRU EOF
           READ INPUT-FILE
               AT END
                   SET EOF TO TRUE
                   GO TO 2000-EXIT
           END-READ

      *    DETERMINE LENGTH OF INPUT INT
           MOVE 1 TO WS-CLICK-DIGIT-LENGTH
           PERFORM UNTIL IN-CLICKS(1:WS-CLICK-DIGIT-LENGTH) NOT NUMERIC
               MOVE IN-CLICKS(1:WS-CLICK-DIGIT-LENGTH) TO WS-CLICK-AMT 
               ADD 1 TO WS-CLICK-DIGIT-LENGTH
           END-PERFORM

      *    ROTATE DIAL
           IF DIR-LEFT
               SUBTRACT WS-CLICK-AMT FROM WS-DIAL-POS
           ELSE
               ADD      WS-CLICK-AMT TO   WS-DIAL-POS
           END-IF

      *    HANDLE CROSSING 0
           IF WS-DIAL-POS GREATER 99
               PERFORM UNTIL WS-DIAL-POS NOT GREATER 99
                   SUBTRACT 100 FROM WS-DIAL-POS
               END-PERFORM
           END-IF

           IF WS-DIAL-POS LESS 0
               PERFORM UNTIL WS-DIAL-POS NOT LESS 0
                   ADD 100 TO   WS-DIAL-POS
               END-PERFORM
           END-IF

      *    INCREMENT OUTPUT IF LANDED AT 0
           IF WS-DIAL-POS EQUAL 0
               ADD 1 TO OUT-TIMES-AT-ZERO
           END-IF
           .
       2000-EXIT.
           EXIT.

      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
       9000-CLOSE-FILE.
           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.

