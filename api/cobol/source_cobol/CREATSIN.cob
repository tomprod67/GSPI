      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CREATSIN.
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
      -              "txt/create_sinistre_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/create_sinistre_response.txt"
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
         05 SizeOfId Pic x(17).
         05 TypeSinistre Pic x(20).
         05 DateSurvenance pic x(30).
         05 DateFin pic x(30).
         05 Circonstance pic x(220).

       01 TypeOfSinistre Pic x(2).
       01 DateOfSurvenance pic x(10).
       01 DateOfFin pic x(10).
       01 Circonstance2 pic x(200).

       01 SYSTEME-DATE.
           03 AA PIC 99.
           03 MM PIC 99.
           03 JJ PIC 99.

       01 currentYear pic 9(4).
       01 currentDate pic X(10).
       01 date-survenance-valide pic 9.
       01 date-Fin-valide pic 9.
       01 yearLimit pic 9(4).

       01 Survenance.
           05 Jour pic 99.
           05 Mois pic 99.
           05 Annee pic 9(4).

       01 Fin.
           05 Jour pic 99.
           05 Mois pic 99.
           05 Annee pic 9(4).
       01 Souscription.
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
       01 DateSouscriptionTemp pic X(10).
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
           if IFDossierExist equal 0 then
               perform Get-DateSouscription-Contrat
               perform Verify-Date
               display date-survenance-valide
               display date-fin-valide
               if date-survenance-valide = 1 and date-fin-valide = 1
                   then
                       perform Create-Sinistre
                       perform Create-Prestation
               end-if
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
            DateSurvenance of champValeur
            DateFin of champValeur
            Circonstance of champValeur
            ContratId of champValeur
            SizeOfId of champValeur
           end-unstring.

           unstring TypeSinistre of champValeur delimited by ":" into
            trash
            TypeOfSinistre
           end-unstring.
           unstring DateSurvenance of champValeur delimited by ":" into
            trash
            DateOfSurvenance
           end-unstring.
           unstring DateFin of champValeur delimited by ":" into
            trash
            DateOfFin
           end-unstring.

           unstring Circonstance of champValeur delimited by ":" into
            trash
            Circonstance
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

           display ContratId-1.
           display ContratId-2.
           display ContratId-3.
           display ContratId-4.
           display ContratId of champValeur.
           display SizeOfId of champValeur.
           display IdSize.
           display DateFin of champValeur.
           display DateFin.
           display "lala"DateOfFin.

       Unstring-Line-Fin.
           EXIT.

      ******************************************************************
      *****                    CHECK-IF-EXIST                      *****
      ******************************************************************
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
               display "la"SQLCODE.
               display SQLCA-STATEMENT.

           EVALUATE SQLCODE
               WHEN 0
                   CONTINUE
               WHEN 100
                   MOVE "ERREUR = VOTRE CONTRAT NE COMPORTE PAS DE DOSSI
      -             "ER AVEC CE TYPE DE SINISTRE"
                   TO MESSAGE-RESPONSE
                   MOVE "ERREUR" TO STATUT-RESPONSE
                   move 1 to IFDossierExist
               WHEN OTHER
                   move 9 to IFDossierExist
                   MOVE "ERREUR = UNE ERREUR SQL NON GEREE EST SURVENUE"
                   TO MESSAGE-RESPONSE
                   MOVE "ERREUR" TO STATUT-RESPONSE
           END-EVALUATE.

       Check-If-Dossier-Exist-Fin.
           perform close-BDD.
           EXIT.

      ******************************************************************
      *****                    Get DateSouscription                *****
      ******************************************************************
       Get-DateSouscription-Contrat.
           perform Get-DateSouscription-Contrat-Init.
           perform Get-DateSouscription-Contrat-Trt.
           perform Get-DateSouscription-Contrat-Fin.

       Get-DateSouscription-Contrat-Init.
           move 0 to IFDossierExist.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).


       Get-DateSouscription-Contrat-Trt.
           perform Generate-Select-Contrat-SQLCA-STATEMENT.
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
                                            DateSouscriptionTemp

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.


       Get-DateSouscription-Contrat-Fin.
           perform close-BDD.
           EXIT.
      ******************************************************************
      *****                      VERIFY-DATE                       *****
      ******************************************************************
       Verify-Date.
           perform Verify-Date-Init.
           perform Verify-Date-Trt.
           perform Verify-Date-Fin.

       Verify-Date-Init.
           ACCEPT SYSTEME-DATE FROM DATE.

           STRING "20" DELIMITED SIZE
                  AA DELIMITED SIZE
           INTO currentYear
           END-STRING.
           STRING JJ DELIMITED SIZE
                  '/' DELIMITED SIZE
                  MM DELIMITED SIZE
                  '/' DELIMITED SIZE
                  currentYear
           INTO currentDate
           END-STRING.
           unstring DateOfSurvenance delimited by "/"
           into
            Jour of Survenance
            Mois of Survenance
            Annee of Survenance
           end-unstring.
           unstring DateOfFin delimited by "/"
           into
            Jour of Fin
            Mois of Fin
            Annee of Fin
           end-unstring.
           unstring DateSouscriptionTemp delimited by "/"
           into
            Jour of Souscription
            Mois of Souscription
            Annee of Souscription
           end-unstring.
           move 1 to date-survenance-valide.
           move 1 to date-fin-valide.
           add 47 to Annee of Souscription giving yearLimit.
       Verify-Date-Trt.
           display Jour of Souscription.
            display Mois of Souscription.
            display Annee of Souscription.
            display Jour of Fin.
            display Mois of Fin.
            display Annee of Fin.
            display DateOfFin.

           if Jour of Survenance > 0 and <= 31 then
               if Mois of Survenance > 0 and <= 12 then
                   if annee of survenance >= Annee of Souscription
                       and <= yearLimit then
                        move 1 to date-survenance-valide
                   else
                       move 0 to date-survenance-valide
                   end-if
               else
                   move 0 to date-survenance-valide
               end-if
           else
               move 0 to date-survenance-valide
           end-if.

           if Jour of Fin > 0 and <= 31 then
               if Mois of Fin > 0 and <= 12 then
                   if annee of Fin >= Annee of Souscription
                       and <= yearLimit then
                           move 1 to date-fin-valide
                   else
                       move 0 to date-fin-valide
                   end-if
               else
                   move 0 to date-fin-valide
               end-if
           else
               move 0 to date-fin-valide
           end-if.

           If date-survenance-valide equal 0 then
               MOVE "ERREUR = LA DATE DE SURVENANCE EST INVALIDE."
               TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.
           If date-fin-valide equal 0 then
               MOVE "ERREUR = LA DATE DE FIN EST INVALIDE."
               TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.

       Verify-Date-Fin.
           EXIT.
      ******************************************************************
      *****                    CREATE-SINISTRE                     *****
      ******************************************************************
       Create-Sinistre.
           perform Create-Sinistre-Init.
           perform Create-Sinistre-Trt.
           perform Create-Sinistre-Fin.

       Create-Sinistre-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Create-Sinistre-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Create-Sinistre-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.


       Create-Sinistre-Fin.
           perform Close-BDD.
           EXIT.
      ******************************************************************
      *****                    CREATE-PRESTA                       *****
      ******************************************************************
       Create-Prestation.
           perform Create-Prestation-Init.
           perform Create-Prestation-Trt.
           perform Create-Prestation-Fin.

       Create-Prestation-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

       Create-Prestation-Trt.
           MOVE 0 TO SQLCODE.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           perform Generate-Create-Presta-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.

           MOVE RETURN-CODE TO SQLCODE.
           display "klalalal:"SQLCODE.
           if SQLCODE equal 0 then
           MOVE "SUCCES = LE SINISTRE A BIEN ETE AJOUTER A VOTRE DOSSIER
      -     " ET EN ATTENTE D'ETRE TRAITER"
           TO MESSAGE-RESPONSE
           MOVE "SUCCES" TO STATUT-RESPONSE
           end-if.
           if SQLCODE is not equal 0 then
               MOVE "ERROR = UNE ERREUR SQL NON GEREE EST SURVENUE."
               TO MESSAGE-RESPONSE
               MOVE "ERROR" TO STATUT-RESPONSE
           end-if.

       Create-Prestation-Fin.
           perform Close-BDD.
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
      *****             GENERATE-IFEXISTE-SQLCA-STATEMENT          *****
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
      *****           GENERATE-SELECT-CONTRAT-SQLCA-STATEMENT      *****
      ******************************************************************
       Generate-Select-Contrat-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'DATESOUSCRIPTION ' DELIMITED SIZE
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
                   'DATESOUSCRIPTION ' DELIMITED SIZE
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
                   'DATESOUSCRIPTION ' DELIMITED SIZE
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
                   'DATESOUSCRIPTION ' DELIMITED SIZE
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
       Generate-Select-Contrat-SQLCA-STATEMENT-Fin.
           EXIT.
      ******************************************************************
      *****          GENERATE-CREATE-DOSSIER-SQLCA-STATEMENT       *****
      ******************************************************************
       Generate-Create-Sinistre-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'SINISTRES ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'DOSSIERID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATEDECLARATION, '    DELIMITED SIZE
                  'DATESURVENANCE, '    DELIMITED SIZE
                  'DATEFIN, '    DELIMITED SIZE
                  'CIRCONSTANCE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '(('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDDOSSIER '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'DOSSIER '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CONTRATID '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  ContratId-1    DELIMITED SIZE
                  '" AND '    DELIMITED SIZE
                  'TYPESINISTRE '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '"),"'  DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  '1'  DELIMITED SIZE
                  '","' DELIMITED SIZE
                  currentDate DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfSurvenance DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfFin DELIMITED SIZE
                  '","' DELIMITED SIZE
                  Circonstance DELIMITED SIZE
                  '"' DELIMITED SIZE
                  ')' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'SINISTRES ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'DOSSIERID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATEDECLARATION, '    DELIMITED SIZE
                  'DATESURVENANCE, '    DELIMITED SIZE
                  'DATEFIN, '    DELIMITED SIZE
                  'CIRCONSTANCE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '(('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDDOSSIER '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'DOSSIER '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CONTRATID '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  ContratId-2    DELIMITED SIZE
                  '" AND '    DELIMITED SIZE
                  'TYPESINISTRE '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '"),"'  DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  '1'  DELIMITED SIZE
                  '","' DELIMITED SIZE
                  currentDate DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfSurvenance DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfFin DELIMITED SIZE
                  '","' DELIMITED SIZE
                  Circonstance DELIMITED SIZE
                  '"' DELIMITED SIZE
                  ')' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'SINISTRES ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'DOSSIERID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATEDECLARATION, '    DELIMITED SIZE
                  'DATESURVENANCE, '    DELIMITED SIZE
                  'DATEFIN, '    DELIMITED SIZE
                  'CIRCONSTANCE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '(('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDDOSSIER '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'DOSSIER '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CONTRATID '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  ContratId-3    DELIMITED SIZE
                  '" AND '    DELIMITED SIZE
                  'TYPESINISTRE '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '"),"'  DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  '1'  DELIMITED SIZE
                  '","' DELIMITED SIZE
                  currentDate DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfSurvenance DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfFin DELIMITED SIZE
                  '","' DELIMITED SIZE
                  Circonstance DELIMITED SIZE
                  '"' DELIMITED SIZE
                  ')' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'SINISTRES ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'DOSSIERID, ' DELIMITED SIZE
                  'TYPESINISTRE, ' DELIMITED SIZE
                  'STATUS, '    DELIMITED SIZE
                  'DATEDECLARATION, '    DELIMITED SIZE
                  'DATESURVENANCE, '    DELIMITED SIZE
                  'DATEFIN, '    DELIMITED SIZE
                  'CIRCONSTANCE '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '(('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'IDDOSSIER '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'DOSSIER '    DELIMITED SIZE
                  'WHERE '    DELIMITED SIZE
                  'CONTRATID '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  ContratId-4    DELIMITED SIZE
                  '" AND '    DELIMITED SIZE
                  'TYPESINISTRE '    DELIMITED SIZE
                  '= "'    DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '"),"'  DELIMITED SIZE
                  TypeOfSinistre  DELIMITED SIZE
                  '","'    DELIMITED SIZE
                  '1'  DELIMITED SIZE
                  '","' DELIMITED SIZE
                  currentDate DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfSurvenance DELIMITED SIZE
                  '","' DELIMITED SIZE
                  DateOfFin DELIMITED SIZE
                  '","' DELIMITED SIZE
                  Circonstance DELIMITED SIZE
                  '"' DELIMITED SIZE
                  ')' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING

           end-evaluate.
       Generate-Create-Sinistre-SQLCA-STATEMENT-Fin.
           EXIT.
      ******************************************************************
      *****          GENERATE-CREATE-PRESTA-SQLCA-STATEMENT       *****
      ******************************************************************
       Generate-Create-Presta-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'PRESTATION ' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'SINISTREID, ' DELIMITED SIZE
                  'STATUS '    DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '(('    DELIMITED SIZE
                  'SELECT '    DELIMITED SIZE
                  'MAX(IDSINISTRE) '    DELIMITED SIZE
                  'FROM '    DELIMITED SIZE
                  'SINISTRES '    DELIMITED SIZE
                  '),"'  DELIMITED SIZE
                  '0'  DELIMITED SIZE
                  '"'    DELIMITED SIZE
                  ')' DELIMITED SIZE
               INTO SQLCA-STATEMENT
           END-STRING.
       Generate-Create-Presta-SQLCA-STATEMENT-Fin.
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
           MOVE 'CREATSIN' TO PGCTB-PROGRAM-NAME.

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
