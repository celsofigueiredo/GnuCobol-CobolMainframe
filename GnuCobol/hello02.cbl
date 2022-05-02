      *>*****************************************************************
      *> Author: Celso
      *> Date: 01/06/2018
      *> Purpose:
      *> Tectonics: cobc
      *>*****************************************************************
       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       hello02.

       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SPECIAL-NAMES.

      *>   CRT STATUS IS escape
           DECIMAL-POINT IS COMMA.

       DATA              DIVISION.

       WORKING-STORAGE   SECTION.

       77  TECLA             PIC  9(01).
       77  WS-FIM            PIC  9(01)    VALUE ZERO.
       77  WS-DATA           PIC  x(50)    VALUE SPACES.

       01  WS-DATA-R.
           03 WS-DIA         PIC  9(02).
           03 FILLER         PIC  X(01) VALUE '/'.
           03 WS-MES         PIC  9(02).
           03 FILLER         PIC  X(01) VALUE '/'.
           03 WS-ANO         PIC  9(04).
           03 FILLER         PIC  X(03) VALUE ' - '.
           03 WS-HORA        PIC  9(02).
           03 FILLER         PIC  X(01) VALUE ':'.
           03 WS-MIN         PIC  9(02).
           03 FILLER         PIC  X(01) VALUE ':'.
           03 WS-SEG         PIC  9(02).

       SCREEN            SECTION.

       01  TELA.
           02 filler pic x(1920)
              BLANK SCREEN line 1 column 1
              foreground-color 7 background-color 1 value spaces.
           02 LINE 02 COLUMN 21
              foreground-color 7 background-color 1
              VALUE " Programa Hello World ".
           02 LINE 03 COLUMN 21
              foreground-color 7 background-color 1
              VALUE " Autor: Celso - FAST ".
           02 LINE 04 COLUMN 21
              foreground-color 7 background-color 1
              VALUE " Inicio do programa Hello02 ".

       01  TELA1.
           02 LINE 14 COLUMN 17
              foreground-color 1 background-color 7
              VALUE "Hello Word                                   ".

       PROCEDURE         DIVISION.

           PERFORM 100-inicio    THRU 100-s.
           PERFORM 200-processa  THRU 200-s.
           PERFORM 300-finaliza  THRU 300-s.
           STOP RUN.

       100-inicio.
      *>
           DISPLAY TELA.

           PERFORM 110-inic-data-hora THRU 110-s.

       100-s.
           EXIT.

       110-inic-data-hora.
      *>
           MOVE function current-date TO WS-DATA.

           MOVE WS-DATA(07:2)  TO WS-DIA
           MOVE WS-DATA(05:2)  TO WS-MES
           MOVE WS-DATA(01:4)  TO WS-ANO
           MOVE WS-DATA(09:2)  TO WS-HORA
           MOVE WS-DATA(11:2)  TO WS-MIN
           MOVE WS-DATA(13:2)  TO WS-SEG.

           DISPLAY 'DATA DO PROCESSAMENTO : ' AT 0617.
           DISPLAY WS-DATA-R AT 0641.

       110-s.
           EXIT.

        200-processa.
      *>
           DISPLAY TELA1.
           DISPLAY 'DATA DO PROCESSAMENTO : ' AT 1517 with
           foreground-color 1 background-color 7.
           DISPLAY WS-DATA-R AT 1541 with
           foreground-color 1 background-color 7.

       200-s.
           EXIT.

       300-finaliza.
      *>
           DISPLAY 'FIM DO PROGRAMA' AT 2431.
           ACCEPT TECLA.

       300-s.
           EXIT.
       END PROGRAM hello02.
