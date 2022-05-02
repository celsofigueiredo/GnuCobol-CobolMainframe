      *>*****************************************************************
      *> Author: Celso
      *> Date: 01/06/2018
      *> Purpose:
      *> Tectonics: cobc
      *>*****************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID. hello01.

       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                DIVISION.
       FILE                SECTION.
       WORKING-STORAGE     SECTION.

       77  TECLA PIC 9(1).

       SCREEN              SECTION.

       01  TELA.
           02  filler pic x(1920)
               BLANK SCREEN line 1 column 1
               foreground-color 7 background-color 1 value spaces.
           02 LINE 04 COLUMN 21
              foreground-color 7 background-color 1
              VALUE "Programa Hello World!".
           02 LINE 06 COLUMN 21
              foreground-color 7 background-color 1
              VALUE "AUTOR:Celso - Fast: 01/06/2018".
           02 LINE 14 COLUMN 21
              foreground-color 7 background-color 1
              VALUE "Olah mundo!".

       PROCEDURE           DIVISION.

       INICIO.
           DISPLAY TELA.
       FIM.

           DISPLAY 'FIM DO PROGRAMA' AT 2431.
           ACCEPT TECLA.
           STOP RUN.
       END PROGRAM hello01.
