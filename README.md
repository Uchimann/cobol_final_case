# COBOL FINAL CASE

Programımız bir ana program (MAINPROG) ve bir alt program (SUBPROG) içerir. Veri dosyasında (INP-FILE) bulunan kayıtları okuyarak başındaki harflere göre işlemler gerçekleştiriyoruz. 
R = Read 
U = Update
W = Write
D = Delete

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


