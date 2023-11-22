       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE005.
      *AUTHOR: FELYPE DANTAS DOS SANTOS.
      ***********************************
      *CADASTRO DE PEDIDOS
      ***********************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPED ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS PED-COD
                  ALTERNATE RECORD KEY IS PED-COD-PRO WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-CLI-CPFCNPJ WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-FOR-CPFCNPJ WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.


           SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.

           SELECT ARQPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PRO-CODIGO
                    ALTERNATE RECORD KEY IS PRO-DESCRICAO
                                   WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.


           SELECT ARQCLI ASSIGN TO DISK
                     ORGANIZATION IS INDEXED
                     ACCESS MODE  IS DYNAMIC
                     RECORD KEY   IS CLI-CPFCNPJ
                     ALTERNATE RECORD KEY IS CLI-CODIGO WITH DUPLICATES
                     ALTERNATE RECORD KEY IS CLI-NOME WITH DUPLICATES
                     FILE STATUS  IS ST-ERRO.

           SELECT ARQFOR ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS FOR-CODIGO
                      ALTERNATE RECORD KEY IS FOR-NOME WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPED.DAT".
       01 REGPED.
          03 PED-COD                 PIC 9(06).
          03 PED-TIPOPEDIDO          PIC X(15).
          03 PED-COD-PRO             PIC 9(09).
          03 PED-TIPOVENDA           PIC 9(01).
          03 PED-CLI-CPFCNPJ         PIC 9(14).
          03 PED-FOR-CPFCNPJ         PIC 9(14).
          03 PED-QUANTIDADE          PIC 9(06).
          03 PED-PRECOUNITARIO       PIC 9(08).
          03 PED-VALORTOTAL          PIC 9(09).
      *-----------------------------------------------------------------
       FD ARQPRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".
       01 REGPRO.
          03 PRO-CODIGO            PIC 9(06).
          03 PRO-DESCRICAO           PIC X(30).
          03 PRO-UNIDADE             PIC X(02).
          03 PRO-TIPOPRODUTO         PIC 9(01).
          03 PRO-PRECO               PIC 9(06).
          03 PRO-DATAULTIMA          PIC 9(08).
          03 PRO-QTDESTOQUE          PIC 9(06).
          03 PRO-CPFCNPJ             PIC 9(14).
      *-----------------------------------------------------------------
       FD ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).
      *-----------------------------------------------------------------

       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
          03 CLI-CODIGO              PIC 9(06).
          03 CLI-CPFCNPJ             PIC 9(14).
          03 CLI-TIPOCLIENTE         PIC X(01).
          03 CLI-NOME                PIC X(30).
          03 CLI-APELIDO             PIC X(30).
          03 CLI-CEP                 PIC 9(08).
          03 CLI-LOGRADOURO          PIC X(24).
          03 CLI-NUMERO              PIC X(11).
          03 CLI-COMPLEMENTO         PIC X(24).
          03 CLI-BAIRRO              PIC X(40).
          03 CLI-CIDADE              PIC X(24).
          03 CLI-ESTADO              PIC X(02).
          03 CLI-TELEFONE            PIC 9(11).
          03 CLI-EMAIL               PIC X(33).
          03 CLI-CONTATO             PIC X(32).
      *-----------------------------------------------------------------
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
          03 CADASTRO.
            05 FOR-CODIGO            PIC 9(06).
          03 FOR-TIPOCLIENTE         PIC X(01).
          03 FOR-CPF                 PIC 9(11).
          03 FOR-CNPJ                PIC 9(14).
          03 FOR-NOME                PIC X(30).
          03 FOR-APELIDO             PIC X(30).
          03 FOR-CEP                 PIC 9(08).
          03 FOR-LOGRADOURO          PIC X(24).
          03 FOR-NUMERO              PIC X(11).
          03 FOR-COMPLEMENTO         PIC X(24).
          03 FOR-BAIRRO              PIC X(40).
          03 FOR-CIDADE              PIC X(24).
          03 FOR-ESTADO              PIC X(02).
          03 FOR-TELEFONE            PIC 9(11).
          03 FOR-EMAIL               PIC X(33).
          03 FOR-CONTATO             PIC X(32).


      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       01 MASCARA            PIC 9(06)V99.
       01 W-OPCAO            PIC X(01) VALUE SPACES.
       01 W-UPPERCASE        PIC X(01) VALUE SPACES.
       01 W-ACT              PIC 9(02) VALUE ZEROS.
       01 ST-ERRO            PIC X(02) VALUE "00".
       01 DPED-TIPOPEDIDO    PIC X(20) VALUE SPACES.
       01 DPRO-DESCRICAO       PIC X(20) VALUE SPACES.
       01 W-CONT             PIC 9(06) VALUE ZEROS.
       01 MENS               PIC X(50) VALUE SPACES.
       01 W-SEL              PIC 9(01) VALUE ZEROS.
       01 IND                PIC 9(02) VALUE ZEROS.
       01 TABTIPOPRODUTO.
          03 TBTIPOPRODUTO   PIC X(30) OCCURS 10 TIMES.

       01 PED-PRO-DESCRICAO PIC X(30) VALUE SPACES.
       01 PED-FOR-CODIGO    PIC 9(06) VALUE ZEROS.

       01 DFOR-ESTADO            PIC X(15) VALUE SPACES.
       01 DFOR-TIPOCLIENTE       PIC X(20) VALUE SPACES.

       01 PED-CLI-NOME PIC X(30) VALUE SPACES.
       01 PED-CPFCNPJ-FOR PIC X(30) VALUE SPACES.
       01 PED-FOR-NOME PIC X(30) VALUE SPACES.

       01 TXUF.
            03 UF-SIGLA     PIC X(02) VALUE SPACES.
            03 UF-TEXTO   PIC X(20) VALUE SPACES.

       01 TBUF.
             03 FILLER     PIC X(22) VALUE "ACACRE".
             03 FILLER     PIC X(22) VALUE "ALALAGOAS".
             03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
             03 FILLER     PIC X(22) VALUE "APAMAPA".
             03 FILLER     PIC X(22) VALUE "BABAHIA".
             03 FILLER     PIC X(22) VALUE "CECEARA".
             03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
             03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
             03 FILLER     PIC X(22) VALUE "GOGOIAS".
             03 FILLER     PIC X(22) VALUE "MAMARANHÃO".
             03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
             03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
             03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
             03 FILLER     PIC X(22) VALUE "PAPARA".
             03 FILLER     PIC X(22) VALUE "PBPARAIBA".
             03 FILLER     PIC X(22) VALUE "PRPARANÁ".
             03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
             03 FILLER     PIC X(22) VALUE "PIPIAUI".
             03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
             03 FILLER     PIC X(22) VALUE "RNR. GRANDE DO NORTE".
             03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
             03 FILLER     PIC X(22) VALUE "RORONDONIA".
             03 FILLER     PIC X(22) VALUE "RRRORAIMA".
             03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
             03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
             03 FILLER     PIC X(22) VALUE "SESERGIPE".
             03 FILLER     PIC X(22) VALUE "TOTOCANTINS".

       01 TBUFR REDEFINES TBUF.
             03 TABUF    PIC X(22) OCCURS 27 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01 TELAPRODUTO.
           05  LINE 01  COLUMN 01
               VALUE  "                           Cadastro".
           05  LINE 01  COLUMN 41
               VALUE  "de Pedidos                         ".
           05  LINE 02  COLUMN 01
               VALUE  "  Codigo Pedido    :".
           05  LINE 02  COLUMN 41
               VALUE  "                                        ".
           05  LINE 03  COLUMN 01
               VALUE  "  Tipo Pedido      :  -".
           05  LINE 03  COLUMN 41
               VALUE  "                                        ".
           05  LINE 04  COLUMN 01
               VALUE  "  Codigo Produto   :".
           05  LINE 04  COLUMN 41
               VALUE  "                                        ".
           05  LINE 05  COLUMN 01
               VALUE  "  Descricao Produto:".
           05  LINE 05  COLUMN 41
               VALUE  "                                        ".
           05  LINE 06  COLUMN 01
               VALUE  "  Codigo Cliente   :".
           05  LINE 06  COLUMN 41
               VALUE  "                                        ".
           05  LINE 07  COLUMN 01
               VALUE  "  Nome Cliente     :".
           05  LINE 07  COLUMN 41
               VALUE  "                                        ".
           05  LINE 08  COLUMN 01
               VALUE  "  Codigo Fornecedor:".
           05  LINE 08  COLUMN 41
               VALUE  "                                        ".
           05  LINE 09  COLUMN 01
               VALUE  "                    ".
           05  LINE 09  COLUMN 41
               VALUE  "                                        ".
           05  LINE 11  COLUMN 01
               VALUE  "  Quantidade       :".
           05  LINE 11  COLUMN 41
               VALUE  "                                        ".
           05  LINE 12  COLUMN 01
               VALUE  "  Preco Unitario   :".
           05  LINE 12  COLUMN 41
               VALUE  "                                        ".
           05  LINE 14  COLUMN 01
               VALUE  "  Valor Total      :".
           05  LINE 14  COLUMN 41
               VALUE  "                                        ".
           05  LINE 16  COLUMN 01
               VALUE  "  Notificacoes:".
           05  LINE 16  COLUMN 41
               VALUE  "                                        ".
           05  LINE 18  COLUMN 01
               VALUE  "                                   Infor".
           05  LINE 18  COLUMN 41
               VALUE  "macoes                                  ".

           05  TPED-COD
               LINE 02  COLUMN 22  PIC 9(06)
               USING  PED-COD.
           05  TPED-TIPOPEDIDO
               LINE 03  COLUMN 22  PIC X(01)
               USING  PED-TIPOPEDIDO.
           05  TDPED-TIPOPEDIDO
               LINE 03  COLUMN 24  PIC X(10)
               USING  DPED-TIPOPEDIDO.

           05  TPED-COD-PRO
               LINE 04  COLUMN 22  PIC 9(06)
               USING  PED-COD-PRO.

           05  TPED-PRO-DESCRICAO
               LINE 05  COLUMN 22  PIC X(30)
               USING  PRO-DESCRICAO.



           05  TPED-CLI-CPFCNPJ
               LINE 06  COLUMN 22  PIC 9(14)
               USING  PED-CLI-CPFCNPJ.

           05  TPED-CLI-NOME
               LINE 07  COLUMN 22  PIC X(30)
               USING  CLI-NOME.
           05  TPED-CPFCNPJ-FOR
               LINE 08  COLUMN 22  PIC X(14)
               USING  PRO-CPFCNPJ.
           05  TPED-FOR-NOME
               LINE 09  COLUMN 22  PIC X(30)
               USING  PED-FOR-NOME.
           05  TPED-QUANTIDADE
               LINE 11  COLUMN 22  PIC 9(06)
               USING  PED-QUANTIDADE.
           05  TPED-PRECOUNITARIO
               LINE 12  COLUMN 22  PIC 9(06)V99
               USING  PRO-PRECO.
           05  TPED-VALORTOTAL
               LINE 14  COLUMN 22  PIC 9(08)V99
               USING  PED-VALORTOTAL.

       01  TELATIPOPRODUTO.
           05  LINE 19  COLUMN 41
               VALUE  "                                        ".
           05  LINE 20  COLUMN 01
               VALUE  "          1-SAUDE                     2-".
           05  LINE 20  COLUMN 41
               VALUE  "ALIMENTO          3-AGROPECUARIA        ".
           05  LINE 21  COLUMN 01
               VALUE  "          4-PET                       5-".
           05  LINE 21  COLUMN 41
               VALUE  "AAAAAAAA          6-BBBBBBBBBBBB        ".
           05  LINE 22  COLUMN 01
               VALUE  "          7-CCCCCCCC                  8-".
           05  LINE 22  COLUMN 41
               VALUE  "DDDDDDDD          9-FFFFFFFFFFFF        ".
           05  LINE 23  COLUMN 01
               VALUE  " ".
           05  LINE 23  COLUMN 41
               VALUE  "                                        ".

       01  TELAUNIDADE.
           05  LINE 19  COLUMN 41
               VALUE  "                                        ".
           05  LINE 20  COLUMN 01
               VALUE  "                                  PC-PEC".
           05  LINE 20  COLUMN 41
               VALUE  "A                                       ".
           05  LINE 21  COLUMN 01
               VALUE  "                                  LT-LIT".
           05  LINE 21  COLUMN 41
               VALUE  "RO                                      ".
           05  LINE 22  COLUMN 01
               VALUE  "                                  KL-KIL".
           05  LINE 22  COLUMN 41
               VALUE  "O                                       ".
           05  LINE 23  COLUMN 01
               VALUE  "                                  TN-TON".
           05  LINE 23  COLUMN 41
               VALUE  "ELADA                                   ".

       01  TELALIMPAR.
           05  LINE 19  COLUMN 01
               VALUE  "                                              ".
           05  LINE 19  COLUMN 41
               VALUE  "                                              ".
           05  LINE 20  COLUMN 01
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41
               VALUE  "                                               ".
           05  LINE 21  COLUMN 01
               VALUE  "                                               ".
           05  LINE 21  COLUMN 41
               VALUE  "                                              ".
           05  LINE 22  COLUMN 01
               VALUE  "                                               ".
           05  LINE 22  COLUMN 41
               VALUE  "                                               ".
           05  LINE 23  COLUMN 01
               VALUE  "                                               ".
           05  LINE 23  COLUMN 41
               VALUE  "                                               ".
           05  LINE 24  COLUMN 01
               VALUE  "                                               ".
           05  LINE 24  COLUMN 41
               VALUE  "                                               ".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
        INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.

           OPEN I-O ARQPED
           IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQPED
              MOVE "CRIANDO ARQUIVO DE PEDIDOS " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPRO
              GO TO INICIO
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CDAMIGOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE PROUTOS NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PRODUTOS " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN INPUT ARQCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                MOVE "*** ARQUIVO DE CLIENTES NAO ENCONTRADO **" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTES " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0C.
           OPEN I-O ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQCEP
                      CLOSE ARQCEP
                      MOVE "* ARQUIVO CEP SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R0C
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.

       R0D.
           OPEN I-O ARQFOR
           IF ST-ERRO NOT = "00"


            IF ST-ERRO = "30"
              OPEN OUTPUT ARQFOR
              MOVE "CRIANDO ARQUIVO CADASTRO DE FUNCIONARIOS " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQFOR
              GO TO R0D
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CDAMIGOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO PED-TIPOPEDIDO DPED-TIPOPEDIDO

           MOVE ZEROS TO PED-COD PED-COD-PRO PED-TIPOVENDA
           PED-CLI-CPFCNPJ PED-FOR-CPFCNPJ PED-QUANTIDADE
           PED-PRECOUNITARIO PED-VALORTOTAL

           MOVE SPACES TO PRO-DESCRICAO PRO-UNIDADE

           MOVE ZEROS TO PRO-CODIGO PRO-TIPOPRODUTO PRO-PRECO
           PRO-DATAULTIMA PRO-QTDESTOQUE PRO-CPFCNPJ.

           MOVE SPACES TO
           CLI-TIPOCLIENTE
           CLI-NOME CLI-APELIDO CLI-LOGRADOURO CLI-NUMERO
           CLI-COMPLEMENTO CLI-BAIRRO CLI-CIDADE CLI-ESTADO
           CLI-EMAIL CLI-CONTATO.

           MOVE ZEROS TO CLI-CODIGO CLI-CPFCNPJ CLI-CEP
           CLI-TELEFONE

           MOVE SPACES TO TXUF CEP-UF CEP-LOGRA CEP-BAIRRO.
           MOVE SPACES TO CEP-CIDADE.
           MOVE ZEROS  TO CEP-NUMCEP.

           MOVE SPACES TO
           DFOR-TIPOCLIENTE DFOR-ESTADO FOR-TIPOCLIENTE
           FOR-NOME FOR-APELIDO FOR-LOGRADOURO FOR-NUMERO
           FOR-COMPLEMENTO FOR-BAIRRO FOR-CIDADE FOR-ESTADO
           FOR-EMAIL FOR-CONTATO.

           MOVE ZEROS TO FOR-CODIGO FOR-CPF FOR-CNPJ FOR-CEP
           FOR-TELEFONE PED-FOR-CODIGO.

      *-------------VISUALIZACAO DA TELA ------------------

           DISPLAY TELAPRODUTO.
      *----------ENTRADA DE DADOS---------------

       R2.
           ACCEPT TPED-COD.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE ARQPED
                  STOP RUN.
           IF PED-COD = SPACES
              MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       LER-ARQPED.
           READ ARQPED
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R3A
                PERFORM R5A THRU R5B
                PERFORM R4A THRU R4B
      *          DISPLAY TELAPRINCIPAL
                MOVE "*** PEDIDO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO PRODUTOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PEDIDO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
           ACCEPT TPED-TIPOPEDIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF PED-TIPOPEDIDO = SPACES
              MOVE "TIPO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R3A.
           IF PED-TIPOPEDIDO = 1
              MOVE "COMPRA" TO DPED-TIPOPEDIDO
            DISPLAY TPED-TIPOPEDIDO
           DISPLAY TELALIMPAR
           DISPLAY TELAPRODUTO

           ELSE
             IF PED-TIPOPEDIDO = 2
                MOVE "VENDA" TO DPED-TIPOPEDIDO
             ELSE


              MOVE "1 - COMPRA : 2 - VENDA" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R3.
           DISPLAY TPED-TIPOPEDIDO.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRODUTO.

       R4.
           ACCEPT TPED-COD-PRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRODUTO
                   GO TO R3.
           IF PED-COD-PRO = 0
               MOVE "*** CODIGO PRODUTO NÃO INFORMADO  ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.

       R4A.
           MOVE PED-COD-PRO TO PRO-CODIGO.
           READ ARQPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
               MOVE "*** PRODUTO DIGITADO NAO ENCONTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
               ELSE
               MOVE "ERRO NA LEITURA ARQUIVO PRODUTO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRODUTO.
       R4B.
               MOVE 1 TO IND.


       R5.
           IF PED-TIPOPEDIDO = 1
              MOVE "COMPRA" TO DPED-TIPOPEDIDO
              MOVE 0 TO PED-CLI-CPFCNPJ
              GO TO R6
           ELSE

              DISPLAY TPED-CLI-CPFCNPJ
           ACCEPT TPED-CLI-CPFCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRODUTO
                   GO TO R3.
           IF PED-CLI-CPFCNPJ = 0
               MOVE "*** CODIGO PRODUTO NÃO INFORMADO  ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.

       R5A.
           MOVE PED-CLI-CPFCNPJ TO CLI-CPFCNPJ.
           READ ARQCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
               MOVE "*** CLIENTE DIGITADO NAO ENCONTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
               ELSE
               MOVE "ERRO NA LEITURA ARQUIVO CLIENTE" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRODUTO.
       R5B.
               MOVE 1 TO IND.

       R6.
           ACCEPT TPED-QUANTIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRODUTO
                   GO TO R4.
           IF PED-QUANTIDADE = 0
                 MOVE "*** QTD ESTOQUE NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.

                 COMPUTE PED-VALORTOTAL = (PRO-PRECO
           * PED-QUANTIDADE).

       R7.
           MOVE PED-VALORTOTAL TO MASCARA
           DISPLAY TPED-VALORTOTAL.
             IF W-SEL = 1
            GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (16, 30) "Deseja Salvar (S/N) : ".
                ACCEPT (16, 55) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                MOVE "* PRODUTO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
       ACE-001.
                DISPLAY (20, 18)
                     "N=NOVO REGISTRO | A=ALTERAR | E=EXCLUIR | S=SAIR:"
                ACCEPT (20, 67) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E" AND W-OPCAO NOT = "S"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (20, 18) MENS
                IF W-OPCAO = "N" OR "n"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A" OR "a"
                      MOVE 1 TO W-SEL
                      GO TO R3
                ELSE
                   IF W-OPCAO = "S" OR "s"
                      MOVE 1 TO W-SEL
                      GO TO ROT-FIM.
      *
       EXC-OPC.
                DISPLAY (16, 30) "Deseja EXCLUIR   (S/N) : ".
                ACCEPT (16, 55) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQPED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (16, 30) "Deseja ALTERAR  (S/N) : ".
                ACCEPT (16, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY

                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.

                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM

                   GO TO R1.
                   DISPLAY TELALIMPAR.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQPED ARQPRO ARQCLI ARQCEP ARQFOR.
           STOP RUN.
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (16, 21) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (16, 21) MENS.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NAO CARREGADO
      *    10 = FIM DA LEITURA ARQUIVO SEQUENCIAL
