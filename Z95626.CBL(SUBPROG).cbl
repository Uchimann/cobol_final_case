       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN TO IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS MODE RANDOM
                             RECORD KEY IDX-KEY
                             STATUS IDX-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
         01  IDX-REC.
           03 IDX-KEY.
             05 IDX-ID                 PIC S9(5)  COMP-3.
             05 IDX-DVZ                PIC S9(3)  COMP.
           03 IDX-NAME                 PIC X(15).
           03 IDX-SRNAME               PIC X(15).
           03 IDX-DATE                 PIC S9(7)  COMP-3.
           03 IDX-BALANCE              PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
         01 I                          PIC 9(3).
         01 J                          PIC 9(2)   VALUE 1.
         01  WS-WORK-AREA.
           05 WS-SUBPROG2              PIC X(08)  VALUE 'SUBPROG'.
           05 IDX-ST                   PIC 9(02).
              88 IDX-SUCCES                       VALUE 00 97.
       LINKAGE SECTION.
         01 WS-SUB-AREA.
              07 WS-PRCSS-TYPE         PIC X(01).
              07 WS-SUB-FUNC           PIC 9(01).
                 88 WS-FUNC-OPEN                  VALUE 1.
                 88 WS-FUNC-READ                  VALUE 2.
                 88 WS-FUNC-UPDATE                VALUE 3.
                 88 WS-FUNC-WRITE                 VALUE 4.
                 88 WS-FUNC-DELETE                VALUE 5.
                 88 WS-FUNC-CLOSE                 VALUE 9.
              07 SUB-OUT-PRCSS-TYPE    PIC X(01).
              07 SUB-OUT-ID            PIC 9(05).
              07 SUB-OUT-DVZ           PIC 9(03).
              07 SUB-OUT-RETURN-CODE   PIC 9(02).
              07 SUB-OUT-DESCRIPTION   PIC X(30).
              07 SUB-OUT-FNAME-FROM    PIC X(15).
              07 SUB-OUT-FNAME-TO      PIC X(15).
              07 SUB-OUT-LNAME-FROM    PIC X(15).
              07 SUB-OUT-LNAME-TO      PIC X(15).
              07 SUB-INP-PRCSS-TYPE    PIC X(01).
              07 SUB-INP-ID            PIC 9(5).
              07 SUB-INP-DVZ           PIC 9(3).

       PROCEDURE DIVISION USING WS-SUB-AREA.
      *-----------------------------------------------------------------
      *    MAIN: Dosyalari acar, basarili acildiysa,islem tipine gore
      *    navigasyon gorevi gorecek olan H400 paragrafina gider.
      *    Islemler bitince H999 paragrafi ile programdan cikar.
      *-----------------------------------------------------------------
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H400-SUBPROG2
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.


      *-----------------------------------------------------------------
      *    H100: Index dosyasini input ve output olarak acar.
      *    Acilma durumunu kontrol eder ve dosya acilmadiysa program
      *    exit paragrafina yonlendirir.
      *-----------------------------------------------------------------
       H100-OPEN-FILES.
           OPEN I-O IDX-FILE.
           IF (IDX-ST NOT = 0) AND (IDX-ST NOT = 97)
               DISPLAY 'UNABLE TO OPEN IDXFILE: ' IDX-ST
               MOVE IDX-ST TO RETURN-CODE
               PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.


      *-----------------------------------------------------------------
      *    H400:Veri dosyasindan bir kayit okur ve kayit mevcutsa
      *    H220-VALID-KEY paragrafina, mevcut degilse H210-INVALID-KEY
      *    paragrafina yonlendirir.
      *-----------------------------------------------------------------
       H400-SUBPROG2.
           MOVE SUB-INP-ID                  TO IDX-ID.
           MOVE SUB-INP-DVZ                 TO IDX-DVZ.

           READ IDX-FILE KEY IS IDX-KEY
                INVALID KEY PERFORM H210-INVALID-KEY
                NOT INVALID KEY PERFORM H220-VALID-KEY.
       H400-END. EXIT.


      *-----------------------------------------------------------------
      *    H210:Islem tipi 'W' degilse isim ve soyisim alanlarina sifir
      *    doldurur. ID bulunamadi mesajini verir.
      *    Islem tipi 'W' ise kaydi eklemek icin H770 paragrafina gider.
      *-----------------------------------------------------------------
       H210-INVALID-KEY.
           MOVE SUB-INP-PRCSS-TYPE          TO WS-PRCSS-TYPE

           IF WS-PRCSS-TYPE = 'W'
              PERFORM H770-WRITE
           ELSE
              MOVE SUB-INP-PRCSS-TYPE       TO SUB-OUT-PRCSS-TYPE
              MOVE SUB-INP-ID               TO SUB-OUT-ID
              MOVE SUB-INP-DVZ              TO SUB-OUT-DVZ
              MOVE IDX-ST                   TO SUB-OUT-RETURN-CODE
              MOVE ZEROES                   TO SUB-OUT-FNAME-FROM
              MOVE ZEROES                   TO SUB-OUT-FNAME-TO
              MOVE ZEROES                   TO SUB-OUT-LNAME-FROM
              MOVE ZEROES                   TO SUB-OUT-LNAME-TO
                    STRING 'ERR: ID BULUNAMADI         :  '
                    DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION
           END-IF.
       H210-END. EXIT.


      *-----------------------------------------------------------------
      *    H220:Islem turune gore WS-SUB-FUNC'in TRUE degeri aranir.
      *    TRUE olan WS-SUB-FUNC degerine göre ilgili paragraflara
      *    yonlendirilir. Gecersiz islem durumunda hata mesaji verilir.
      *-----------------------------------------------------------------
       H220-VALID-KEY.
           MOVE SUB-INP-PRCSS-TYPE          TO WS-PRCSS-TYPE

           IF WS-PRCSS-TYPE = 'R'
              SET WS-FUNC-READ TO TRUE
           ELSE IF WS-PRCSS-TYPE = 'D'
              SET WS-FUNC-DELETE TO TRUE
           ELSE IF WS-PRCSS-TYPE = 'W'
              SET WS-FUNC-WRITE TO TRUE
           ELSE IF WS-PRCSS-TYPE = 'U'
              SET WS-FUNC-UPDATE TO TRUE
           END-IF.

           EVALUATE TRUE
              WHEN WS-FUNC-READ
                PERFORM H760-READ
              WHEN WS-FUNC-DELETE
                PERFORM H750-DELETE
              WHEN WS-FUNC-WRITE
                 PERFORM H770-RECORD-FOUND
              WHEN WS-FUNC-UPDATE
                 PERFORM H700-UPDATE
              WHEN OTHER
                 STRING 'ERR: GECERSIZ ISLEM        :  '
                 DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION
                 DISPLAY 'GECERSIZ ISLEM'
           END-EVALUATE.
       H220-END. EXIT.


      *-----------------------------------------------------------------
      *    H700:Okunan isimdeki bosluklari atlayarak, SUB-OUT-FNAME-TO
      *    degiskenine kopyalanir. Okunan soyisimdeki 'E'-'I', sonrasida
      *    'A'-'E' donusumleri yapilir. REWRITE ile kayit guncellenir.
      *    J degiskeni ile okunan bosluk sayisi mesaj olarak verilir.
      *-----------------------------------------------------------------
       H700-UPDATE.
           MOVE ZEROES                      TO SUB-OUT-FNAME-FROM
           MOVE ZEROES                      TO SUB-OUT-LNAME-FROM
           MOVE IDX-NAME                    TO SUB-OUT-FNAME-FROM
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-FROM
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-TO

           COMPUTE J = 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF IDX-NAME
                   IF IDX-NAME(I:1) NOT EQUAL SPACE
                      MOVE IDX-NAME(I:1) TO SUB-OUT-FNAME-TO(J:1)
                      COMPUTE J = J + 1
                   END-IF
           END-PERFORM.

           INSPECT IDX-SRNAME REPLACING
                   ALL 'E' BY 'I'
           INSPECT IDX-SRNAME REPLACING
                   ALL 'A' BY 'E'

           MOVE SUB-OUT-FNAME-TO            TO IDX-NAME
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-TO
           REWRITE IDX-REC
           END-REWRITE

           COMPUTE J = (15 - J)
           MOVE SUB-INP-PRCSS-TYPE          TO SUB-OUT-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO SUB-OUT-ID
           MOVE SUB-INP-DVZ                 TO SUB-OUT-DVZ
           MOVE IDX-ST                      TO SUB-OUT-RETURN-CODE
           STRING 'REC. UPDATED SPACE COUNT   :'J
                DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION.
       H700-END. EXIT.


      *-----------------------------------------------------------------
      *    H770:ID bulunamadi ise ve islem tipi 'W' ise calisir.
      *    Isim degiskenine 'ISMAIL'soyisim degiskenine 'CELEBI' atilir.
      *-----------------------------------------------------------------
       H770-WRITE.
           MOVE SUB-INP-ID                  TO IDX-ID
           MOVE SUB-INP-DVZ                 TO IDX-DVZ
           MOVE 'ISMAIL'                    TO IDX-NAME
           MOVE 'CELEBI'                    TO IDX-SRNAME
           WRITE IDX-REC.

           MOVE SUB-INP-PRCSS-TYPE          TO SUB-OUT-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO SUB-OUT-ID
           MOVE SUB-INP-DVZ                 TO SUB-OUT-DVZ
           MOVE 'ISMAIL'                    TO SUB-OUT-FNAME-FROM
           MOVE SPACES                      TO SUB-OUT-FNAME-TO
           MOVE 'CELEBI'                    TO SUB-OUT-LNAME-FROM
           MOVE SPACES                      TO SUB-OUT-LNAME-TO
           MOVE IDX-ST                      TO SUB-OUT-RETURN-CODE.
           STRING 'YAZMA GERCEKLESTI          :  '
               DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION.
       H770-END. EXIT.


      *-----------------------------------------------------------------
      *    H770:Islem tipi 'W' ise ve ID bulunur ise calisir. Kayit
      *    eklenmedi zaten var mesaji verilir.
      *-----------------------------------------------------------------
       H770-RECORD-FOUND.
           MOVE SUB-INP-PRCSS-TYPE          TO SUB-OUT-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO SUB-OUT-ID
           MOVE SUB-INP-DVZ                 TO SUB-OUT-DVZ
           MOVE IDX-NAME                    TO SUB-OUT-FNAME-FROM
           MOVE SPACES                      TO SUB-OUT-FNAME-TO
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-FROM
           MOVE SPACES                      TO SUB-OUT-LNAME-TO
           MOVE IDX-ST                      TO SUB-OUT-RETURN-CODE.
           STRING 'EKLENMEDI. ID ZATEN VAR    :  '
               DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION.
       H770-END. EXIT.


      *-----------------------------------------------------------------
      *    H760:ID bulundu ise, okunan kayit, basarili okuma mesaji ile
      *    birlikte SUB-OUT bolumlerine atilir.
      *-----------------------------------------------------------------
       H760-READ.
           MOVE SUB-INP-PRCSS-TYPE          TO SUB-OUT-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO SUB-OUT-ID
           MOVE SUB-INP-DVZ                 TO SUB-OUT-DVZ
           MOVE IDX-ST                      TO SUB-OUT-RETURN-CODE
           STRING 'BASARILI OKUMA             :  '
               DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION.
           MOVE IDX-NAME                    TO SUB-OUT-FNAME-FROM
           MOVE SPACES                      TO SUB-OUT-FNAME-TO
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-FROM.
           MOVE SPACES                      TO SUB-OUT-LNAME-TO.
       H760-END. EXIT.


      *-----------------------------------------------------------------
      *    H750:Okunan kaydin silme islemini gerceklestirir.
      *    Hata durumunu kontrol eder.
      *-----------------------------------------------------------------
       H750-DELETE.
           MOVE SUB-INP-PRCSS-TYPE          TO SUB-OUT-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO SUB-OUT-ID
           MOVE SUB-INP-DVZ                 TO SUB-OUT-DVZ
           MOVE IDX-ST                      TO SUB-OUT-RETURN-CODE
           MOVE IDX-NAME                    TO SUB-OUT-FNAME-FROM
           MOVE SPACES                      TO SUB-OUT-FNAME-TO
           MOVE IDX-SRNAME                  TO SUB-OUT-LNAME-FROM
           MOVE SPACES                      TO SUB-OUT-LNAME-TO.

             DELETE IDX-FILE RECORD
                 IF IDX-ST = 00
                      STRING 'SILME GERCEKLESTI          :  '
                      DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION
                 ELSE
                      STRING 'SILME GERCEKLESMEDI        :  '
                      DELIMITED BY SIZE INTO SUB-OUT-DESCRIPTION
                 END-IF.
       H750-END. EXIT.


      *-----------------------------------------------------------------
      *    Index dosyasini kapatir ve programin cagrildigi yere doner.
      *-----------------------------------------------------------------
       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           GOBACK.
      *
