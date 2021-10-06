      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CHECKSIN.
       AUTHOR.                     Thomas.


      **************************************************************************
      *E N V I R O N M E N T    D I V I S I O N                              *
      **************************************************************************
       ENVIRONMENT DIVISION.
       input-output section.

       file-control.
           select F-DataSubmited
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/check_sinistre_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/check_sinistre_response.txt"
           organization is line sequential access sequential.
      **************************************************************************
      *D A T A    D I V I S I O N                                            *
      **************************************************************************
       DATA DIVISION.

       FILE SECTION.

       FD F-DataSubmited record varying from 0 to 255.
       01 E-DataSubmited pic x(255).

       FD F-Response record varying from 0 to 1000.
       01 E-Response pic x(1000).
      **************************************************************************
      *W O R K I N G   S T O R A G E   S E C T I O N                         *
      **************************************************************************
       WORKING-STORAGE SECTION.

       01 Boucleur-read-file pic 9.

       01 idcl1 pic x.
         88 idcl1-bool value 1.

       01 idcl2 pic x.
         88 idcl2-bool value 1.

       01 idcl3 pic x.
         88 idcl3-bool value 1.

       01 idcl4 pic x.
         88 idcl4-bool value 1.

       01 idco1 pic x.
         88 idco1-bool value 1.

       01 idco2 pic x.
         88 idco2-bool value 1.

       01 idco3 pic x.
         88 idco3-bool value 1.

       01 idco4 pic x.
         88 idco4-bool value 1.

       01 champValeur.
         05 ClientId Pic x(15).
         05 SizeOfIdCli Pic x(20).
         05 ContratId Pic x(15).
         05 SizeOfIdCon Pic x(20).

       01 trash pic X(255).
       01 CheckCorrespond pic 9.

       01 idCliSize pic 9.
       01 idConSize pic 9.

       01 ClientId-1 pic 9.
       01 ClientId-2 pic 99.
       01 ClientId-3 pic 999.
       01 ClientId-4 pic 9999.

       01 ContratId-1 pic 9.
       01 ContratId-2 pic 99.
       01 ContratId-3 pic 999.
       01 ContratId-4 pic 9999.

       01 BASE-OF-SQLCA-STATEMENT pic X(102).

       01 Cli-Temp-Statement pic X(4).
       01 Con-Temp-Statement pic X(3).

       01 Client.
           05 IdCli pic 9(4).
           05 Nom Pic X(30).
           05 Prenom Pic X(30).
           05 DateNaissance Pic X(15).
           05 Adresse Pic X(100).
           05 CodePostal Pic X(5).
           05 Ville Pic X(30).
           05 Age pic 99.

       01 Contrat.
           05 IdCon pic 9(4).
           05 TypeSinistre pic X(2).
           05 StatusCon pic 9(1).
           05 DateSouscription pic X(10).
           05 PrixParMois pic X(8).

       01 MontantG pic Z(8).
       01 MESSAGE-RESPONSE pic X(80).
       01 STATUT-RESPONSE pic X(6).

       01 DATA-RESPONSE pic X(1500).
       01 CLIENT-STRING pic X(380).
       01 CONTRAT-STRING pic X(160).
       01 DOSSIER-STRING.
           10 ONE-DOSSIER OCCURS 7 PIC X(110).
       01 COMPLETE-RESPONSE pic X(2000).

       01 contrat-present pic 9 value 0.
       01 indexDos pic 9 value 0.
       01 delimite pic X(2) value "}}".


       COPY CPYTOM OF "cobol/source_cobol".
      ******************************************************************
      *P R O C E D U R E   D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION.
       GSPI.
           perform GSPI-Init.
           perform GSPI-Trt.
           perform GSPI-Fin.

       GSPI-Init.
           perform Read-File-Submited.

       GSPI-Trt.
           perform Check-If-Correspond.
           if CheckCorrespond equal 0 then
               perform Select-Contrat-Data
           end-if.
           perform Write-Response-File.
       GSPI-Fin.
           stop run.

       Read-File-Submited.
           perform Read-File-Submited-Init.
           perform Read-File-Submited-Trt until Boucleur-read-file = 1.
           perform Read-File-Submited-Fin.

       Read-File-Submited-Init.
           move 0 to Boucleur-read-file.
           open INPUT F-DataSubmited.

       Read-File-Submited-Trt.
           read F-DataSubmited
               at end
                   move 1 to Boucleur-read-file
               not at end
                     perform Unstring-Line
           end-read.

       Read-File-Submited-Fin.
           close F-DataSubmited.

       Unstring-Line.
           unstring E-DataSubmited delimited by "," or space into
            ClientId of champValeur
            SizeOfIdCli of champValeur
            ContratId of champValeur
            SizeOfIdCon of champValeur
           end-unstring.
           unstring SizeOfIdCli of champValeur delimited by ":" into
            trash
            idCliSize
           end-unstring.
           unstring SizeOfIdCon of champValeur delimited by ":" into
            trash
            idConSize
           end-unstring.

           EVALUATE idCliSize
             WHEN 1
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-1
               end-unstring
               SET idcl1-bool TO TRUE
             WHEN 2
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-2
               end-unstring
               SET idcl2-bool TO TRUE
             WHEN 3
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-3
               end-unstring
               SET idcl3-bool TO TRUE
             WHEN 4
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-4
               end-unstring
               SET idcl4-bool TO TRUE
           end-evaluate.

           EVALUATE idConSize
             WHEN 1
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-1
               end-unstring
               SET idco1-bool TO TRUE
             WHEN 2
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-2
               end-unstring
               SET idco2-bool TO TRUE
             WHEN 3
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-3
               end-unstring
               SET idco3-bool TO TRUE
             WHEN 4
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-4
               end-unstring
               SET idco4-bool TO TRUE
           end-evaluate.


       Unstring-Line-Fin.
           EXIT.

       Check-If-Correspond.
           perform Check-If-Correspond-Init.
           perform Check-If-Correspond-Trt.
           perform Check-If-Correspond-Fin.

       Check-If-Correspond-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.
           MOVE 0 TO SQLCODE.
           MOVE 0 to CheckCorrespond.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Select-If-Correspond-SQLCA-STATEMENT.
       Check-If-Correspond-Trt.
           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
           IF DB-OK
              CALL 'MySQL_use_result' USING SQLCA-RESULT (1)
              END-CALL
              IF SQLCA-RESULT (1) = NULL
                 MOVE 100 TO SQLCODE
              ELSE
                 MOVE 0 TO SQLCODE
              END-IF
           END-IF.

           IF DB-OK
               CALL 'MySQL_fetch_row' USING SQLCA-RESULT (1)
                                            IdCli of Client
                                            Nom of Client
                                            Prenom of Client
                                            DateNaissance of Client
                                            Adresse of Client
                                            CodePostal of Client
                                            Ville of Client
                                            Age of Client

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
           IF SQLCODE equal 0 then
               STRING  '"client" : ' DELIMITED SIZE
                       '{' DELIMITED SIZE
                       '"id" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       IdCli of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"nom" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Nom of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"prenom" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Prenom of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"dateNaissance" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       DateNaissance of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"adresse" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Adresse of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"codePostal" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       CodePostal of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"ville" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Ville of Client DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"age" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Age of Client DELIMITED SIZE
                       '"' DELIMITED SIZE                      
                       '}' DELIMITED SIZE
                   INTO CLIENT-STRING
               END-STRING
           END-IF.
           IF SQLCODE equal 100 then
               MOVE 1 to CheckCorrespond
               MOVE "ERREUR = NUMERO CLIENT ET CONTRAT NE CORRESPONDENT
      -         "PAS"
               TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.
           DISPLAY "SQLCODE SELECT IFEXIST :"SQLCODE.


       Check-If-Correspond-Fin.
           perform Close-BDD.
           EXIT.

       Select-Contrat-Data.
           perform Select-Contrat-Data-Init.
           perform Select-Contrat-Data-Trt.
           perform Select-Contrat-Data-Fin.


       Select-Contrat-Data-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Select-Contrat.
       Select-Contrat-Data-Trt.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           IF DB-OK
              CALL 'MySQL_use_result' USING SQLCA-RESULT (1)
              END-CALL
              IF SQLCA-RESULT (1) = NULL
                 MOVE 100 TO SQLCODE
              ELSE
                 MOVE 0 TO SQLCODE
              END-IF
           END-IF.

           IF DB-OK
               CALL 'MySQL_fetch_row' USING SQLCA-RESULT (1)
                                            IdCon of Contrat
                                            TypeSinistre of Contrat
                                            StatusCon of Contrat
                                            DateSouscription of Contrat
                                            PrixParMois of Contrat

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
           IF SQLCODE equal 0 then
               STRING  '"contrat" : ' DELIMITED SIZE
                   '{' DELIMITED SIZE
                   '"id" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   IdCon of Contrat DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"typeSinistre" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   TypeSinistre of Contrat DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"statusCon" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   StatusCon of Contrat DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"dateSouscription" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   DateSouscription of Contrat DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"PrixParMois" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   PrixParMois of Contrat DELIMITED SIZE
                   '"' DELIMITED SIZE
                   '}' DELIMITED SIZE
                   INTO CONTRAT-STRING
               END-STRING
           END-IF.
           DISPLAY "SQLCODE SELECT CONTRAT :"SQLCODE.
           IF SQLCODE EQUAL 0 THEN
               MOVE "SUCCES = LES IDENTIFIANTS CORRESPONDENT"
               TO MESSAGE-RESPONSE
               MOVE "SUCCES" TO STATUT-RESPONSE
           END-IF.

       Select-Contrat-Data-Fin.
           perform Close-BDD.
           EXIT.

       Write-Response-File.
           open output F-Response.
           if CheckCorrespond equal 0 then
               STRING '{' DELIMITED SIZE
                       CLIENT-STRING DELIMITED SIZE
                       ',' DELIMITED SIZE
                       CONTRAT-STRING DELIMITED SIZE
                       '' DELIMITED SIZE
                   INTO DATA-RESPONSE
               END-STRING
               STRING '{' DELIMITED SIZE
                       '"statut" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       STATUT-RESPONSE DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"message" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       MESSAGE-RESPONSE DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"data" : ' DELIMITED SIZE
                       DATA-RESPONSE DELIMITED SIZE
               INTO COMPLETE-RESPONSE
               END-STRING
               write E-Response from COMPLETE-RESPONSE
               write E-Response from delimite
           else
               STRING '{' DELIMITED SIZE
                   '"statut" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   STATUT-RESPONSE DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"message" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   MESSAGE-RESPONSE DELIMITED SIZE
                   '"' DELIMITED SIZE
                   '}' DELIMITED SIZE
               INTO COMPLETE-RESPONSE
               END-STRING
               write E-Response from COMPLETE-RESPONSE     
           END-IF.
           display COMPLETE-RESPONSE.

           close F-Response.
           EXIT.
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                GENERATE SQLCA STATEMENT                *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
       Generate-Select-If-Correspond-SQLCA-STATEMENT.
           evaluate TRUE
           when idcl1-bool and idco1-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-1 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-1 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl1-bool and idco2-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-1 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-2 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl1-bool and idco3-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-1 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-3 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl2-bool and idco1-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-2 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-1 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl2-bool and idco2-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-2 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-2 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl2-bool and idco3-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-2 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-3 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl3-bool and idco1-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-3 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-1 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl3-bool and idco2-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-3 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-2 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idcl3-bool and idco3-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCLIENT, ' DELIMITED SIZE
                       'NOM, ' DELIMITED SIZE
                       'PRENOM, ' DELIMITED SIZE
                       'DATENAISSANCE, ' DELIMITED SIZE
                       'ADRESSE, ' DELIMITED SIZE
                       'CODEPOSTAL, '    DELIMITED SIZE
                       'VILLE, '    DELIMITED SIZE
                       'AGE '    DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CLIENTS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCLIENT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ClientId-3 DELIMITED SIZE
                       '" ' DELIMITED SIZE
                       'AND ' DELIMITED SIZE
                       'CONTRATID ' DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-3 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           END-EVALUATE.
               display SQLCA-statement.

       Generate-Select-Contrat-SQLCA-STATEMENT.
           EXIT.

       Generate-Select-Contrat.


           evaluate TRUE
           when idco1-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCONTRAT, ' DELIMITED SIZE
                       'TYPESINISTRE, ' DELIMITED SIZE
                       'STATUS, ' DELIMITED SIZE
                       'DATESOUSCRIPTION, ' DELIMITED SIZE
                       'PRIXPARMOIS ' DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CONTRATS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCONTRAT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-1 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idco2-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCONTRAT, ' DELIMITED SIZE
                       'TYPESINISTRE, ' DELIMITED SIZE
                       'STATUS, ' DELIMITED SIZE
                       'DATESOUSCRIPTION, ' DELIMITED SIZE
                       'PRIXPARMOIS ' DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CONTRATS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCONTRAT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-2 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           when idco3-bool
               STRING 'SELECT ' DELIMITED SIZE
                       'IDCONTRAT, ' DELIMITED SIZE
                       'TYPESINISTRE, ' DELIMITED SIZE
                       'STATUS, ' DELIMITED SIZE
                       'DATESOUSCRIPTION, ' DELIMITED SIZE
                       'PRIXPARMOIS ' DELIMITED SIZE
                       'FROM '    DELIMITED SIZE
                       'CONTRATS '    DELIMITED SIZE
                       'WHERE '    DELIMITED SIZE
                       'IDCONTRAT '    DELIMITED SIZE
                       '= "' DELIMITED SIZE
                       ContratId-3 DELIMITED SIZE
                       '"' DELIMITED SIZE
                   INTO SQLCA-STATEMENT
               END-STRING
           END-EVALUATE.
           DISPLAY ";"SQLCA-STATEMENT";".

       Generate-Select-Contrat-Fin.
           EXIT.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                      UTILITAIRES                       *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

      ******************************************************************
      *****              INITIALISATION-CONNEXION-BDD              *****
      ******************************************************************
       Initialisation-connexion-BDD.
           PERFORM Initialisation-connexion-BDD-Init.
           PERFORM Initialisation-connexion-BDD-Trt.
           PERFORM Initialisation-connexion-BDD-Fin.

       Initialisation-connexion-BDD-Init.
           MOVE 'CHECKSIN' TO PGCTB-PROGRAM-NAME.

           SET PGCTB-OK TO TRUE.
           SET DB-OK TO TRUE.

      *    Recuperation paramètre de connexion à la BDD
           CALL "read_params"         USING PGCTB-PROGRAM-NAME
                                            SQLCA-HOST
                                            SQLCA-USER
                                            SQLCA-PASSWD
                                            SQLCA-DBNAME
                                            SQLCA-PORT
                                            SQLCA-SOCKET
           END-CALL.

           INSPECT SQLCA-HOST   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-USER   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-PASSWD REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-DBNAME REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-PORT   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-SOCKET REPLACING ALL LOW-VALUE BY SPACE.

       Initialisation-connexion-BDD-Trt.
      ***** Initialize the database connection *****

           CALL "MySQL_init"  USING SQLCA-CID
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
       Initialisation-connexion-BDD-Fin.
           EXIT.

      ******************************************************************
      *****                    CONNEXION-DBD                       *****
      ******************************************************************
       Connexion-BDD.
      ***** Conection à la BDD *****
           CALL "MySQL_real_connect" USING
                                   SQLCA-HOST
                                   SQLCA-USER
                                   SQLCA-PASSWD
                                   SQLCA-DBNAME
                                   SQLCA-PORT
                                   SQLCA-SOCKET
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.

       Connexion-BDD-Fin.
           EXIT.

      ******************************************************************
      *****                    CLOSE-BDD                           *****
      ******************************************************************
       Close-BDD.
           CALL "MySQL_close"
           END-CALL.
           EXIT.
