      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CREATDOS.
       AUTHOR.                     Thomas.


      **************************************************************************
      *E N V I R O N M E N T    D I V I S I O N                              *
      **************************************************************************
       ENVIRONMENT DIVISION.
       configuration section.
       special-names.
           Decimal-Point is Comma.
       input-output section.

       file-control.
           select F-DataSubmited
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/creation_dossier_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/creation_dossier_response.txt"
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

       01 id1 pic x.
         88 id1-bool value 1.

       01 id2 pic x.
         88 id2-bool value 1.

       01 id3 pic x.
         88 id3-bool value 1.

       01 id4 pic x.
         88 id4-bool value 1.

       01 champValeur.
         05 ContratId Pic x(15).
         05 SizeOfId Pic x(11).
         05 TypeSinistre Pic x(15).
         05 MontantGarantie pic x(25).


       01 TypeOfSinistre pic X(2).
       01 GarantieMontant pic 9(8).
       01 GarantieM pic Z(8).


       01 ClientDateNaissance Pic X(15).
       01 SYSTEME-DATE.
           03 AA PIC 99.
           03 MM PIC 99.
           03 JJ PIC 99.

       01 currentYear pic 9(4).
       01 Naissance.
           05 Jour pic 99.
           05 Mois pic 99.
           05 Annee pic 9(4).
       01 AgeOfCli pic 99.
       01 age pic 99.
       01 AgeMax pic 99 value 65.
       01 DiffAge pic 99.
       01 PrixContratTemp pic 9999V99.
       01 AncienPrix pic 9999V99.
       01 PrixParMoisInt pic 9999V99.
       01  PrixParMoisFinal pic Z(4),99.

       01 IdDossierTemp pic 9(4).
       01 IFDossierExist pic 9.

       01 trash pic X(255).

       01 idSize pic 9.

       01 ContratId-1 pic 9.
       01 ContratId-2 pic 99.
       01 ContratId-3 pic 999.
       01 ContratId-4 pic 9999.


       01 MESSAGE-RESPONSE pic X(150).
       01 STATUT-RESPONSE pic X(6).

       01 COMPLETE-RESPONSE pic X(1000).


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
           perform Check-If-Dossier-Exist.
       GSPI-Trt.
           if IFDossierExist equal 1 then
               perform Create-Dossier
               perform Get-Infos-Client
               perform Calcul-Age
               perform Select-Ancien-Prix-Contrat
               perform Calcul-Prix-Contrat
               if AgeOfCli is not equal age then
                   perform Update-Age-Client
               end-if
               perform Update-Prix-Contrat
           end-if.
           perform Write-Response-File.
       GSPI-Fin.
           stop run.


      ******************************************************************
      *****                    READ-FILE-SUBMITED                  *****
      ******************************************************************
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

      ******************************************************************
      *****                    UNSTRING-LINE                       *****
      ******************************************************************
       Unstring-Line.
           display E-DataSubmited.
           unstring E-DataSubmited delimited by "," or space into
            TypeSinistre of champValeur
            MontantGarantie of champValeur
            ContratId of champValeur
            SizeOfId of champValeur
           end-unstring.

           unstring TypeSinistre of champValeur delimited by ":" into
            trash
            TypeOfSinistre
           end-unstring.
           unstring MontantGarantie of champValeur delimited by ":" into
            trash
            GarantieMontant
           end-unstring.
           unstring SizeOfId of champValeur delimited by ":" into
            trash
            idSize
           end-unstring.

           EVALUATE idSize
             WHEN 1
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-1
               end-unstring
               SET id1-bool TO TRUE
             WHEN 2
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-2
               end-unstring
               SET id2-bool TO TRUE
             WHEN 3
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-3
               end-unstring
               SET id3-bool TO TRUE
             WHEN 4
               unstring ContratId of champValeur delimited by ":" into
               trash
               ContratId-4
               end-unstring
               SET id4-bool TO TRUE
           end-evaluate.
                         display         TypeSinistre of champValeur.
           display MontantGarantie of champValeur.
            display ContratId of champValeur.
            display SizeOfId of champValeur.
           display ContratId-1.
           display ContratId-2.
           display ContratId-3.
           display ContratId-4.

       Unstring-Line-Fin.
           EXIT.


       Check-If-Dossier-Exist.
           perform Check-If-Dossier-Exist-Init.
           perform Check-If-Dossier-Exist-Trt.
           perform Check-If-Dossier-Exist-Fin.

       Check-If-Dossier-Exist-Init.
           move 0 to IFDossierExist.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).


       Check-If-Dossier-Exist-Trt.
           perform Generate-IfExist-SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.
           display SQLCA-STATEMENT.
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
                                            IdDossierTemp

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE "ERREUR = UN DOSSIER AVEC CE TYPE DE SINISTRE EX
      -             "ISTE DEJA SUR LE CONTRAT RENSEIGNE"
                   TO MESSAGE-RESPONSE
                   MOVE "ERREUR" TO STATUT-RESPONSE
                   CONTINUE
               WHEN 100
                   move 1 to IFDossierExist
               WHEN OTHER
                   MOVE "ERREUR = UNE ERREUR SQL NON GEREE EST SURVENUE"
                   TO MESSAGE-RESPONSE
                   MOVE "ERREUR" TO STATUT-RESPONSE
           END-EVALUATE.

       Check-If-Dossier-Exist-Fin.
           perform close-BDD.
           EXIT.

      ******************************************************************
      *****                    CREATE-DOSSIER                      *****
      ******************************************************************
       Create-Dossier.
           perform Create-Dossier-Init.
           perform Create-Dossier-Trt.
           perform Create-Dossier-Fin.

       Create-Dossier-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.
           move GarantieMontant to GarantieM.

       Create-Dossier-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Create-Dossier-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.


           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           display "dossier" SQLCODE.
       Create-Dossier-Fin.
       EXIT.


      ******************************************************************
      *****                    GET-Infos-CLIENT                      *****
      ******************************************************************
       Get-Infos-Client.
           perform Get-Infos-Client-Init.
           perform Get-Infos-Client-Trt.
           perform Get-Infos-Client-Fin.


       Get-Infos-Client-Init.
           perform Initialisation-connexion-BDD.
           perform connexion-BDD.

           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).

       Get-Infos-Client-Trt.
           perform Generate-Select-SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.
           display SQLCA-STATEMENT.
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
                                            ClientDateNaissance
                                            AgeOfCli
               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.

       Get-Infos-Client-Fin.
           perform close-BDD.
           EXIT.


       Calcul-Age.
           perform Calcul-Age-Init.
           perform Calcul-Age-Trt.
           perform Calcul-Age-Fin.

       Calcul-Age-Init.
           ACCEPT SYSTEME-DATE FROM DATE.

           STRING "20" DELIMITED SIZE
                  AA DELIMITED SIZE
           INTO currentYear
           END-STRING.

       Calcul-Age-Trt.
           subtract Annee of Naissance from currentYear giving age.

           if MM of SYSTEME-DATE < Mois of Naissance then
               subtract 1 from age
           end-if.
           if MM of SYSTEME-DATE equal Mois of Naissance then
               if JJ of SYSTEME-DATE < Jour of Naissance then
                   subtract 1 from age
               end-if
           end-if.

       Calcul-Age-Fin.

           EXIT.
       Select-Ancien-Prix-Contrat.
           perform Select-Ancien-Prix-Contrat-Init.
           perform Select-Ancien-Prix-Contrat-Trt.
           perform Select-Ancien-Prix-Contrat-Fin.

       Select-Ancien-Prix-Contrat-Init.
           perform Initialisation-connexion-BDD.
           perform connexion-BDD.

           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).

       Select-Ancien-Prix-Contrat-Trt.
           perform Generate-Prix-Contrat-SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.
           display SQLCA-STATEMENT.
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
                                            AncienPrix
               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
           display "laici"AncienPrix.
       Select-Ancien-Prix-Contrat-Fin.
           perform close-BDD.
           EXIT.
      ******************************************************************
      *****                  CALCUL-PRIX-CONTRAT                   *****
      ******************************************************************
       Calcul-Prix-Contrat.
           subtract age from AgeMax giving DiffAge.
           divide DiffAge into GarantieMontant giving PrixContratTemp.
           divide 12 into PrixContratTemp giving PrixParMoisInt.
           display AncienPrix.
           display PrixParMoisInt.
           add AncienPrix to PrixParMoisInt.
           display PrixParMoisInt.
           move PrixParMoisInt to PrixParMoisFinal.
           move GarantieMontant to GarantieM.
           display PrixParMoisFinal.

       Calcul-Prix-Contrat-Fin.
           EXIT.

      ******************************************************************
      *****                    UPDATE-CLIENT-AGE                   *****
      ******************************************************************
       Update-Age-Client.
           perform Update-Age-Client-Init.
           perform Update-Age-Client-Trt.
           perform Update-Age-Client-Fin.

       Update-Age-Client-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Update-Age-Client-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Update-Client-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.


           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           display SQLCODE.
       Update-Age-Client-Fin.
           perform Close-BDD.
           Exit.

      ******************************************************************
      *****                    UPDATE-CONTRAT-PRIX                 *****
      ******************************************************************
       Update-Prix-Contrat.
           perform Update-Prix-Contrat-Init.
           perform Update-Prix-Contrat-Trt.
           perform Update-Prix-Contrat-Fin.

       Update-Prix-Contrat-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Update-Prix-Contrat-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Update-Contrat-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.


           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           display SQLCODE.
           if SQLCODE equal 0 then
           MOVE "SUCCES = UN NOUVEAU DOSSIER A BIEN ETE CREER, LE PRIX D
      -     "U CONTRAT A ETE ADAPTE EN CONSEQUENCE"
           TO MESSAGE-RESPONSE
           MOVE "SUCCES" TO STATUT-RESPONSE
           end-if.
           if SQLCODE is not equal 0 then
               MOVE "ERROR = UNE ERREUR SQL NON GEREE EST SURVENUE."
               TO MESSAGE-RESPONSE
               MOVE "ERROR" TO STATUT-RESPONSE
           end-if.

       Update-Prix-Contrat-Fin.
           perform Close-BDD.
           Exit.

       Write-Response-File.
           open output F-Response.
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
           END-STRING.
           write E-Response from COMPLETE-RESPONSE.
           close F-Response.
           EXIT.
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                GENERATE SQLCA SATTEMENT                *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

      ******************************************************************
      *****             GENERATE-IFEXISTE-SQLCA-STATEMENT            *****
      ******************************************************************
       Generate-IfExist-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDDOSSIER ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'DOSSIER '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                   'TYPESINISTRE ' DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   TypeOfSinistre DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDDOSSIER ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'DOSSIER '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                   'TYPESINISTRE ' DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   TypeOfSinistre DELIMITED SIZE
                   '"' DELIMITED SIZE

               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDDOSSIER ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'DOSSIER '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                   'TYPESINISTRE ' DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   TypeOfSinistre DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDDOSSIER ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'DOSSIER '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                   'TYPESINISTRE ' DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   TypeOfSinistre DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-IfExist-SQLCA-STATEMENT-Fin.
           EXIT.
      ******************************************************************
      *****          GENERATE-CREATE-DOSSIER-SQLCA-STATEMENT       *****
      ******************************************************************
       Generate-Create-Dossier-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'DOSSIER ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CONTRATID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'MONTANTGARANTIE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ContratId-1    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  GarantieM  DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'DOSSIER ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CONTRATID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'MONTANTGARANTIE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ContratId-1    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  GarantieM  DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'DOSSIER ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CONTRATID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'MONTANTGARANTIE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ContratId-1    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  GarantieM  DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'DOSSIER ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CONTRATID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'MONTANTGARANTIE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ContratId-1    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  GarantieM  DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.

      ******************************************************************
      *****             GENERATE-SELECT-SQLCA-STATEMENT            *****
      ******************************************************************
       Generate-Select-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'AGE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'AGE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'AGE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'AGE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-Select-SQLCA-STATEMENT-Fin.
           EXIT.

      ******************************************************************
      *****             GENERATE-UPDATE-CLIENT-SQLCA-STATEMENT     *****
      ******************************************************************
       Generate-Update-Client-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'AGE '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   age DELIMITED SIZE
                   '" WHERE ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'AGE '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   age DELIMITED SIZE
                   '" WHERE ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'AGE '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   age DELIMITED SIZE
                   '" WHERE ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'AGE '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   age DELIMITED SIZE
                   '" WHERE ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           end-evaluate.
       Generate-Update-Client-SQLCA-STATEMENT-Fin.
           EXIT.

      ******************************************************************
      *****             GENERATE-SELECT-CONTRAT-SQLCA-STATEMENT    *****
      ******************************************************************
       Generate-Prix-Contrat-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
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
           when id2-bool
               STRING 'SELECT ' DELIMITED SIZE
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
           when id3-bool
               STRING 'SELECT ' DELIMITED SIZE
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
           when id4-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'PRIXPARMOIS ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CONTRATS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDCONTRAT '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ContratId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-Prix-Contrat-SQLCA-STATEMENT-Fin.
           EXIT.
      ******************************************************************
      *****             GENERATE-UPDATE-CONTRAT-SQLCA-STATEMENT    *****
      ******************************************************************
       Generate-Update-Contrat-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'PRIXPARMOIS '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   PrixParMoisFinal DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' WHERE ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'PRIXPARMOIS '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   PrixParMoisFinal DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' WHERE ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'PRIXPARMOIS '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   PrixParMoisFinal DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' WHERE ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'PRIXPARMOIS '    DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   PrixParMoisFinal DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' WHERE ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ContratId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           end-evaluate.
       Generate-Update-Contrat-SQLCA-STATEMENT-Fin.
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
           MOVE 'CREATDOS' TO PGCTB-PROGRAM-NAME.

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
           display "connection BDD ".
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
