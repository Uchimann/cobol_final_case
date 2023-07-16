# COBOL FINAL CASE

Programımız bir ana program (MAINPROG) ve bir alt program (SUBPROG) içerir. Veri dosyasında (INP-FILE) bulunan kayıtları okuyarak başındaki harflere göre işlemler gerçekleştiriyoruz. 
- R = Read  :Vsam dosyasından okunan veriyi çıktı dosyasına yazdırır.
- U = Update:Vsam dosyasından bulunan verinin isimdeki boşluklarını siler ve soyisimdeki 'a' harflerini 'e', 'e' harflerini 'ı' yapar. Çıktıya yazdırır. Kaydı günceller
- W = Write :Vsam dosyasına yeni bir kayıt ekler. İsmi 'İsmail' Soyismi 'Çelebi' olur. Eklenen kayıt çıktıya yazılır.
- D = Delete:Vsam dosyasında bulunan kaydı siler. Çıktı dosyasına silindi bilgisi ile yazılır.

## Dosyaların Hazırlanması

Programın doğru çalışabilmesi için aşağıdaki dosyaların bulunması gerekiyor. Bu dosyalara erişmek için JCL dosyalarını çalıştırabilirsiniz. Programın Çalıştırılması kısmında JCL dosyalarının sırası ile nasıl çalışması gerektiğini anlattım. 

- `INPFILE`: Giriş veri dosyası. Kayıtların okunacağı dosyamız.
- `OUTFILE`: Çıkış veri dosyası. İşlenen kayıtların yazılacağı dosyamız.
- `IDXFILE`: İndeks veri dosyası. Alt program (SUBPROG) tarafından kullanılan index dosyamız.


## Programın Çalıştırılması

Programın doğru bir şekilde çalışabilmesi için aşağıdaki adımları izleyiniz:

1. `SORTEG02` JCL dosyasını çalıştırarak QSAM.AA ve QSAM.BB dosyalarını oluşturun.
2. `SORTEG03` JCL dosyasını çalıştırarak INPUT fıle'ı oluşturun.
3. `DELDEF01` JCL dosyasını çalıştırarak VSAM.AA dosyasını oluşturun.
4. `SUBCMPJ` JCL dosyasını çalıştırarak programı başlatın ve OUT dosyasını oluşturun.

Program çalıştırıldığında, ana program (MAINPROG) veri dosyalarını açacak, işlemleri gerçekleştirecek ve çıkış verilerini OUTFILE'a yazacak.

## Örnek çalışma

Aşağıda çıktı senaryosunda ne olacağı ile ilgili küçük bir örnek verilmiştir.

### VSAM dosyasındaki veriler

```bash
10001840M E H M E T    AYDIN          1974261000000000000000
10001918MEHMET         AYDIN          1974261000000000000000
10003978ALI            YILMAZ         1996227000000000000000
10004840RAMAZAN        YILMAZ         1996228000000000000000
10004978RAMAZAN        YILMAZ         1996228000000000000000
10008840MU STAFA       YILMAZ         2023152000000000000000
10008949M U S TAFA     YILMAZ         2023152000000000000000
10008978MUSTAFA        YILMAZ         2023152000000000000000
```
### INPUT dosyasındaki veriler

```bash
R10001840
D10001840
D19999999
R10001840
W10001840
R10001840
U10001840
R10001840
W10003978
R10003978
W19075555
R19075555
U19075555
R19075555
U10008949
R10008949
U10008949
R10008949
```
### Çıktı dosyası

```bash
R10001840 RC: 00.BASARILI OKUMA             :   M E H M E T                   AYDIN                         
D10001840 RC: 00.SILME GERCEKLESTI          :   M E H M E T                   AYDIN                         
D19999999 RC: 23.ERR: ID BULUNAMADI         :   000000000000000000000000000000000000000000000000000000000000
R10001840 RC: 23.ERR: ID BULUNAMADI         :   000000000000000000000000000000000000000000000000000000000000
W10001840 RC: 00.YAZMA GERCEKLESTI          :   ISMAIL                        CELEBI                        
R10001840 RC: 00.BASARILI OKUMA             :   ISMAIL                        CELEBI                        
U10001840 RC: 00.REC. UPDATED SPACE COUNT   :08 ISMAIL         ISMAIL         CELEBI         CILIBI         
R10001840 RC: 00.BASARILI OKUMA             :   ISMAIL                        CILIBI                        
W10003978 RC: 00.EKLENMEDI. ID ZATEN VAR    :   ALI                           YILMAZ                        
R10003978 RC: 00.BASARILI OKUMA             :   ALI                           YILMAZ                        
W19075555 RC: 00.YAZMA GERCEKLESTI          :   ISMAIL                        CELEBI                        
R19075555 RC: 00.BASARILI OKUMA             :   ISMAIL                        CELEBI                        
U19075555 RC: 00.REC. UPDATED SPACE COUNT   :08 ISMAIL         ISMAIL         CELEBI         CILIBI         
R19075555 RC: 00.BASARILI OKUMA             :   ISMAIL                        CILIBI                        
U10008949 RC: 00.REC. UPDATED SPACE COUNT   :07 M U S TAFA     MUSTAFA        YILMAZ         YILMEZ         
R10008949 RC: 00.BASARILI OKUMA             :   MUSTAFA                       YILMEZ                        
U10008949 RC: 00.REC. UPDATED SPACE COUNT   :07 MUSTAFA        MUSTAFA        YILMEZ         YILMIZ         
R10008949 RC: 00.BASARILI OKUMA             :   MUSTAFA                       YILMIZ                        
```
