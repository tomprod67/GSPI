      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CREATCON.
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
      -              "txt/creation_contrat_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/creation_contrat_response.txt"
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

       01 SYSTEME-DATE.
         03 AA PIC 99.
         03 MM PIC 99.
         03 JJ    PIC 99.

       01 DateComplete pic X(10).

       01 id1 pic x.
         88 id1-bool value 1.

       01 id2 pic x.
         88 id2-bool value 1.

       01 id3 pic x.
         88 id3-bool value 1.

       01 id4 pic x.
         88 id4-bool value 1.

       01 AgeMax pic 99 value 65.

       01 IdContratTemp pic 9(4).

       01 champValeur.
         05 ClientId Pic x(13).
         05 SizeOfId Pic x(11).
         05 TypeSinistre Pic x(15).
         05 MontantGarantie pic x(25).

       01 TypeOfSinistre pic x(2).
       01 PrixParMoisInt pic 9999V99.

       01 pic X(10).
       01  PrixParMoisFinal pic Z(4),99.

       01 DiffAge pic 99.
       01 PrixContratTemp pic 9999V99.

       01 GarantieMontant pic 9(6).
       01 GarantieFinal pic Z(6).
       01 MontantG pic X(6).
       01 Age pic 99.
       01 StatusContrat pic 9 value 1.


       01 trash pic X(255).

       01 idSize pic 9.

       01 ClientId-1 pic 9.
       01 ClientId-2 pic 99.
       01 ClientId-3 pic 999.
       01 ClientId-4 pic 9999.

       01 Client.
           05 IdCli pic 9(4).
           05 Nom Pic X(30).
           05 Prenom Pic X(30).
           05 DateNaissance Pic X(15).
           05 Adresse Pic X(100).
           05 CodePostal Pic X(5).
           05 Ville Pic X(30).


       01 MESSAGE-RESPONSE pic X(220).
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

       GSPI-Trt.
           perform Get-Infos-Client.
           IF IdContratTemp IS NOT GREATER THAN 0 then
               perform Calcul-Prix-Contrat
               perform Insert-Contrat
               perform Update-Client
               perform Create-Dossier
           END-IF.
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
            ClientId of champValeur
            SizeOfId of champValeur
           end-unstring.
           display ClientId of champValeur.
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
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-1
               end-unstring
               SET id1-bool TO TRUE
             WHEN 2
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-2
               end-unstring
               SET id2-bool TO TRUE
             WHEN 3
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-3
               end-unstring
               SET id3-bool TO TRUE
             WHEN 4
               unstring ClientId of champValeur delimited by ":" into
               trash
               ClientId-4
               end-unstring
               SET id4-bool TO TRUE
           end-evaluate.

       Unstring-Line-Fin.
           EXIT.

      ******************************************************************
      *****                    GET-AGE-CLIENT                      *****
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
                                            Age
                                            IdContratTemp
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

      ******************************************************************
      *****                    INSERT-CONTRAT                      *****
      ******************************************************************
       Insert-Contrat.
           perform Insert-Contrat-Init.
           perform Insert-Contrat-Trt.
           perform Insert-Contrat-Fin.

       Insert-Contrat-Init.

           ACCEPT SYSTEME-DATE FROM DATE.
           STRING  JJ of SYSTEME-DATE DELIMITED SIZE
                   "/" DELIMITED SIZE
                   MM of SYSTEME-DATE DELIMITED SIZE
                   "/20" DELIMITED SIZE
                   AA of SYSTEME-DATE DELIMITED SIZE
               INTO DateComplete
           END-STRING.

           perform Calcul-Prix-Contrat.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Insert-Contrat-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Insert-SQLCA-STATEMENT.


           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.

       Insert-Contrat-Fin.
           perform Close-BDD.
           display SQLCODE.
           EXIT.
      ******************************************************************
      *****                  CALCUL-PRIX-CONTRAT                   *****
      ******************************************************************
       Calcul-Prix-Contrat.
           subtract Age from AgeMax giving DiffAge.
           divide DiffAge into GarantieMontant giving PrixContratTemp.
           divide 12 into PrixContratTemp giving PrixParMoisInt.
           move PrixParMoisInt to PrixParMoisFinal.
           move GarantieMontant to GarantieFinal.
           display PrixParMoisFinal.

       Calcul-Prix-Contrat-Fin.
           EXIT.

      ******************************************************************
      *****                    UPDATE-CLIENT                       *****
      ******************************************************************
       Update-Client.
           perform Update-Client-Init.
           perform Update-Client-Trt.
           perform Update-Client-Fin.

       Update-Client-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Update-Client-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Update-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.


           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           display SQLCODE.
       Update-Client-Fin.
           perform Close-BDD.
           Exit.

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
       if SQLCODE not equal 0 then
           MOVE "ERREUR = UNE ERREUR SQL NON GEREE EST SURVENUE"
           TO MESSAGE-RESPONSE
           MOVE "ERREUR" TO STATUT-RESPONSE
       end-if.
       if SQLCODE equal 0 then
           MOVE "SUCCES = LE CONTRAT POUR CE CLIENT A BIEN ETE CREER.
      -     "Le prix du contrat à été calculé en fonction de votre a
      -      "ge. Un dossier de type de sinistre vide a egalement ét
      -      "é généré et lié à une garantie."
           TO MESSAGE-RESPONSE
           MOVE "SUCCES" TO STATUT-RESPONSE
       end-if.
       EXIT.

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
      *****             GENERATE-SELECT-SQLCA-STATEMENT            *****
      ******************************************************************
       Generate-Select-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'AGE, ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDCLIENT '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ClientId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'AGE, ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDCLIENT '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ClientId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'AGE, ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDCLIENT '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ClientId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'AGE, ' DELIMITED SIZE
                   'CONTRATID ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'CLIENTS '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDCLIENT '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   ClientId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-Select-SQLCA-STATEMENT-Fin.
           EXIT.

      ******************************************************************
      *****             GENERATE-INSERT-SQLCA-STATEMENT            *****
      ******************************************************************
       Generate-Insert-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'CONTRATS ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CLIENTID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATESOUSCRIPTION, '    DELIMITED SIZE
                  'PRIXPARMOIS '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ClientId-1    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  StatusContrat DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateComplete DELIMITED SIZE
                  '","' DELIMITED SIZE
                  PrixParMoisFinal DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'CONTRATS ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CLIENTID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATESOUSCRIPTION, '    DELIMITED SIZE
                  'PRIXPARMOIS '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ClientId-2    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  StatusContrat DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateComplete DELIMITED SIZE
                  '","' DELIMITED SIZE
                  PrixParMoisFinal DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'CONTRATS ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CLIENTID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATESOUSCRIPTION, '    DELIMITED SIZE
                  'PRIXPARMOIS '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ClientId-3    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  StatusContrat DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateComplete DELIMITED SIZE
                  '","' DELIMITED SIZE
                  PrixParMoisFinal DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'CONTRATS ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'CLIENTID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATESOUSCRIPTION, '    DELIMITED SIZE
                  'PRIXPARMOIS '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ClientId-4    DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  StatusContrat DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateComplete DELIMITED SIZE
                  '","' DELIMITED SIZE
                  PrixParMoisFinal DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-Insert-SQLCA-STATEMENT-Fin.
           EXIT.

      ******************************************************************
      *****             GENERATE-UPDATE-SQLCA-STATEMENT            *****
      ******************************************************************
       Generate-Update-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   ' = ' DELIMITED SIZE
                   '(' DELIMITED SIZE
                   'SELECT ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   'FROM ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'CLIENTID' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ')' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'IDCLIENT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   ' = ' DELIMITED SIZE
                   '(' DELIMITED SIZE
                   'SELECT ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   'FROM ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'CLIENTID' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ')' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'IDCLIENT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   ' = ' DELIMITED SIZE
                   '(' DELIMITED SIZE
                   'SELECT ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   'FROM ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'CLIENTID' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ')' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'IDCLIENT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'UPDATE ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'SET '    DELIMITED SIZE
                   'CONTRATID '    DELIMITED SIZE
                   ' = ' DELIMITED SIZE
                   '(' DELIMITED SIZE
                   'SELECT ' DELIMITED SIZE
                   'IDCONTRAT ' DELIMITED SIZE
                   'FROM ' DELIMITED SIZE
                   'CONTRATS ' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'CLIENTID' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ')' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'IDCLIENT ' DELIMITED SIZE
                   ' = "' DELIMITED SIZE
                   ClientId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           end-evaluate.
       Generate-Update-SQLCA-STATEMENT-Fin.
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
                  '('    DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDCONTRAT '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'CONTRATS '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CLIENTID'    DELIMITED SIZE
                  ' = '    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  ClientId-1    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  '),' DELIMITED SIZE
                  '"' DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  GarantieFinal DELIMITED SIZE
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
                  '('    DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDCONTRAT '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'CONTRATS '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CLIENTID'    DELIMITED SIZE
                  ' = '    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  ClientId-2    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  '),' DELIMITED SIZE
                  '"' DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  GarantieFinal DELIMITED SIZE
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
                  '('    DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDCONTRAT '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'CONTRATS '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CLIENTID'    DELIMITED SIZE
                  ' = '    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  ClientId-3    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  '),' DELIMITED SIZE
                  '"' DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  GarantieFinal DELIMITED SIZE
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
                  '('    DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDCONTRAT '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'CONTRATS '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CLIENTID'    DELIMITED SIZE
                  ' = '    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  ClientId-4    DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  '),' DELIMITED SIZE
                  '"' DELIMITED SIZE
                  TypeOfSinistre DELIMITED SIZE
                  '","' DELIMITED SIZE
                  GarantieFinal DELIMITED SIZE
                  '")' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
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
           MOVE 'CREATCON' TO PGCTB-PROGRAM-NAME.

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
