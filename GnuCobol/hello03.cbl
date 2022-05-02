      *>*****************************************************************
      *> Author: Celso
      *> Date: 01/06/2018
      *> Purpose:
      *> Tectonics: cobc
      *>*****************************************************************
       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       hello03.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          CRT STATUS IS WS-FNC-KEY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FNC-KEY                   PIC 9(4).
          88 V-FNC-F1                  VALUE 1001.
          88 V-FNC-F2                  VALUE 1002.
          88 V-FNC-F9                  VALUE 1009.
          88 V-FNC-F10                 VALUE 1010.
       01 WS-ACCEPT-FNC-KEY            PIC X.

       01 WS-MSG.
         02 WS-SQLCODE                 PIC S9(10).
         02 WS-SQLSTATE                PIC X(5).
         02 WS-MSG-1                   PIC X(80).
         02 WS-MSG-2                   PIC X(80).
         02 WS-MSG-3                   PIC X(80).
         02 WS-MSG-4                   PIC X(80).

       01 WS-CONNECT.
         02 WS-DBNAME                  PIC X(20).
         02 WS-USERID                  PIC X(20).
         02 WS-PSWD                    PIC X(20).


      *> colors
       COPY "screenio.cpy".

       SCREEN SECTION.
       01 HEADER-SCREEN.
          05 FILLER LINE 2 COLUMN 13
             VALUE "PostgreSQL sample program, please select a function"
      *>     this deletes the screen
             BLANK SCREEN
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.

       01 MAIN-FUNCTION-SCREEN.
          05 FILLER LINE 4 COLUMN 5
             VALUE "F1 - Connect to PostgreSQL"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 5 COLUMN 5
             VALUE "F2 - Disconnect"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 18 COLUMN 5
             VALUE "F9 - Exit"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.

       01 MESSAGE-SCREEN.
      *> line 20
          05 FILLER LINE 20 COLUMN 1
             VALUE "SQLCODE: "
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC -Z(9)9 FROM WS-SQLCODE OF WS-MSG
             LINE 20 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 20 COLUMN 30
             VALUE "SQLSTATE: "
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X(5) FROM WS-SQLSTATE OF WS-MSG
             LINE 20 COLUMN 40
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
      *> line 21
          05 FILLER PIC X(80) FROM WS-MSG-1 OF WS-MSG
             LINE 21 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
      *> line 22
          05 FILLER PIC X(80) FROM WS-MSG-2 OF WS-MSG
             LINE 22 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
      *> line 23
          05 FILLER PIC X(80) FROM WS-MSG-3 OF WS-MSG
             LINE 23 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
      *> line 24
          05 FILLER PIC X(80) FROM WS-MSG-4 OF WS-MSG
             LINE 24 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.

       01 CONNECT-SCREEN.
          05 FILLER LINE 4 COLUMN 1
             VALUE "DBNAME:"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X(20) TO WS-DBNAME
             LINE 4 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 4 COLUMN 50
             VALUE "eg.: postgres"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 5 COLUMN 1
             VALUE "USERID:"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X(20) TO WS-USERID
             LINE 5 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 5 COLUMN 50
             VALUE "eg.: Laszlo.Erdoes"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 6 COLUMN 1
             VALUE "PSWD:"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X(20) TO WS-PSWD SECURE
             LINE 6 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 6 COLUMN 50
             VALUE "eg.: laszlopw"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 18 COLUMN 1
             VALUE "F1 - Connect to PostgreSQL"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER LINE 18 COLUMN 30
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-WHITE
             background-color COB-COLOR-BLUE.


       PROCEDURE DIVISION.

      *>------------------------------------------------------------------------
       MAIN-PGTEST1 SECTION.
      *>------------------------------------------------------------------------

          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY
             DISPLAY MAIN-FUNCTION-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY
             ACCEPT MAIN-FUNCTION-SCREEN END-ACCEPT

      *>     init message
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY

             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-CONNECT-SCREEN

                WHEN V-FNC-F2
      *>              PERFORM FNC-DISCONNECT

                WHEN V-FNC-F9
                   EXIT PERFORM

                WHEN OTHER
                   MOVE "Please select a valid function key"
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM

          STOP RUN

          .
       MAIN-PGTEST1-EX.
          EXIT.

      *>------------------------------------------------------------------------
       FNC-CONNECT-SCREEN SECTION.
      *>------------------------------------------------------------------------

          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY
             DISPLAY CONNECT-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY
             ACCEPT CONNECT-SCREEN END-ACCEPT

      *>     init message
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY

             EVALUATE TRUE
                WHEN V-FNC-F1
      *>             PERFORM FNC-CONNECT

                WHEN V-FNC-F10
                   EXIT PERFORM

                WHEN OTHER
                   MOVE "Please select a valid function key"
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
           END-PERFORM

      *>     .
      *>      FNC-CONNECT-SCREEN-EX.
      *>       EXIT.

      *>------------------------------------------------------------------------
      *> FNC-CONNECT SECTION.
      *>------------------------------------------------------------------------

      *>    INITIALIZE LN-MOD
      *>    INITIALIZE WS-MSG
      *>    SET V-LN-FNC-CONNECT OF LN-MOD TO TRUE
      *>    MOVE WS-CONNECT TO LN-CONNECT OF LN-MOD

      *>    CALL "PGMOD1" USING LN-MOD END-CALL

      *>    PERFORM COPY-LN-MSG-IN-WS-MSG

      *>    .
      *> FNC-CONNECT-EX.
      *>    EXIT.

      *>------------------------------------------------------------------------
      *>     FNC-DISCONNECT SECTION.
      *>------------------------------------------------------------------------

      *>    INITIALIZE LN-MOD
      *>      INITIALIZE WS-MSG
      *>    SET V-LN-FNC-DISCONNECT OF LN-MOD TO TRUE

      *>    CALL "PGMOD1" USING LN-MOD END-CALL

      *>   PERFORM COPY-LN-MSG-IN-WS-MSG

      *>    .
      *>   FNC-DISCONNECT-EX.
      *>   EXIT.

      *>------------------------------------------------------------------------
      *> COPY-LN-MSG-IN-WS-MSG SECTION.
      *>------------------------------------------------------------------------

      *>    MOVE LN-MSG                  OF LN-OUTPUT
      *>      TO WS-MSG

          .
       COPY-LN-MSG-IN-WS-MSG-EX.
          EXIT.




       END PROGRAM hello03.
