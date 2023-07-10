       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN TO IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS RANDOM
                             RECORD KEY IDX-KEY
                             STATUS ST-IDX-FILE.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
         01  IDX-REC.
           03 IDX-KEY.
             05 IDX-ID          PIC S9(5) COMP-3.
             05 IDX-DVZ         PIC S9(3) COMP.
           03 IDX-NAME          PIC X(15).
           03 IDX-SRNAME        PIC X(15).
           03 IDX-DATE          PIC S9(7) COMP-3.
           03 IDX-BALANCE       PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           03 ST-IDX-FILE       PIC 9(2).
              88 IDX-SUCCES                     VALUE 00 97.
       LINKAGE SECTION.
         01 WS-SUB-AREA.
              07 WS-SUB-FUNC    PIC 9(01).
                 88 WS-FUNC-OPEN                VALUE 1.
                 88 WS-FUNC-READ                VALUE 2.
                 88 WS-FUNC-UPDATE              VALUE 3.
                 88 WS-FUNC-CLOSE               VALUE 9.
              07 WS-SUB-ID      PIC 9(05).
              07 WS-SUB-DVZ     PIC 9(03).
              07 WS-SUB-RC      PIC 9(02).
              07 WS-SUB-DATA    PIC X(60).
       PROCEDURE DIVISION USING WS-SUB-AREA.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           
       0000-END. EXIT.


       H100-OPEN-FILES.
           OPEN INPUT IDX-FILE.
       H100-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           EXIT PROGRAM.
