      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CREATCLI.
       AUTHOR.                     Thomas.


      **************************************************************************
      *E N V I R O N M E N T    D I V I S I O N                              *
      **************************************************************************
       ENVIRONMENT DIVISION.
       input-output section.

       file-control.
           select F-DataSubmited
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/creation_client_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/creation_client_response.txt"
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

       01 Naissance.
           05 Jour pic 99.
           05 Mois pic 99.
           05 Annee pic 9(4).

       01 SYSTEME-DATE.
           03 AA PIC 99.
           03 MM PIC 99.
           03 JJ PIC 99.

       01 currentYear pic 9(4).

       01 age pic 9(2).

       01 champValeur.
         05 ClientNom Pic X(50).
         05 ClientPrenom Pic X(50).
         05 ClientDateNaissance Pic X(50).
         05 ClientAdresse Pic X(110).
         05 ClientCodePostal Pic X(50).
         05 ClientVille Pic X(50).

       01 trash pic X(255).

       01 DataSubmited.
         05 ClientNom Pic X(30).
         05 ClientPrenom Pic X(30).
         05 ClientDateNaissance Pic X(15).
         05 ClientAdresse Pic X(100).
         05 ClientCodePostal Pic X(5).
         05 ClientVille Pic X(30).

       01 field1 pic X(30).
       01 field2 pic X(35).


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
           perform Unstring-Line.
           perform Calcul-Age.
       GSPI-Trt.
           if age >= 18
               perform PGCTB-MAIN
               perform Write-Response-File
           else
               perform Write-Response-File
           end-if.
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
           unstring E-DataSubmited delimited by "," into
            ClientNom of champValeur
            ClientPrenom of champValeur
            ClientDateNaissance of champValeur
            ClientAdresse of champValeur
            ClientCodePostal of champValeur
            ClientVille of champValeur
           end-unstring.
           unstring ClientNom of champValeur delimited by ":" into
            trash
            ClientNom of DataSubmited
           end-unstring.
           unstring ClientPrenom of champValeur delimited by ":" into
            trash
            ClientPrenom of DataSubmited
           end-unstring.
           unstring ClientDateNaissance of champValeur
           delimited by ":" into
            trash
            ClientDateNaissance of DataSubmited
           end-unstring.
           unstring ClientAdresse of champValeur delimited by ":" into
            trash
            ClientAdresse of DataSubmited
           end-unstring.
           unstring ClientCodePostal of champValeur
           delimited by ":" into
            trash
            ClientCodePostal of DataSubmited
           end-unstring.
           unstring ClientVille of champValeur delimited by ":" into
            trash
            ClientVille of DataSubmited
           end-unstring.

           unstring ClientDateNaissance of DataSubmited delimited by "/"
           into
            Jour of Naissance
            Mois of Naissance
            Annee of Naissance
           end-unstring.


       Unstring-Line-Fin.
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
           display age.
           IF age < 18 THEN
               MOVE "ERREUR = Le client doit avoir au minimum 18 ans pou
      -         "r pouvoir etre enregistre" TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.

       Calcul-Age-Fin.

           EXIT.
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                    FRAMEWORK PGCTBBAT                  *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

       PGCTB-MAIN.
      ***** Inititialisation du framework
      * IMPORTANT CHANGER 'CREATCLI' par votre nom de programme

           MOVE 'CREATCLI' TO PGCTB-PROGRAM-NAME.

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

      ***** Initialize the database connection *****

           CALL "MySQL_init"  USING SQLCA-CID
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.

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

      ***** EXECUTE Le code de l'user dans PGCTB-ACTION *****
           PERFORM PGCTB-ACTION THRU PGCTB-ACTION-FIN.

      *    **** Close BDD *****

           CALL "MySQL_close"
           END-CALL.

       PGCTB-MAIN-EXIT.

           EXIT.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                FIN DU FRAMEWORK PGCTBBAT               *****
      ******************************************************************
      ******************************************************************
      ******************************************************************

       PGCTB-ACTION.
           perform Check-If-Client-Exist.

       PGCTB-ACTION-FIN.
           EXIT.

      ******************************************************************
      *****                CHECK-IF-CLIENT-EXIST                   *****
      ******************************************************************
       Check-If-Client-Exist.
           perform Check-If-Client-Exist-Init.
           perform Check-If-Client-Exist-Trt.
           perform Check-If-Client-Exist-Fin.

       Check-If-Client-Exist-Init.
           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).

           MOVE LOW-VALUES TO SQLCA-STATEMENT.

           STRING 'SELECT ' DELIMITED SIZE
                   'IDCLIENT ' DELIMITED SIZE
                   'FROM ' DELIMITED SIZE
                   'CLIENTS ' DELIMITED SIZE
                   'WHERE ' DELIMITED SIZE
                   'NOM '    DELIMITED SIZE
                   '="' DELIMITED SIZE
                   ClientNom of DataSubmited DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                   'PRENOM ' DELIMITED SIZE
                    '="' DELIMITED SIZE
                   ClientPrenom of DataSubmited DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                    'DATENAISSANCE ' DELIMITED SIZE
                    '="' DELIMITED SIZE
                   ClientDateNaissance of DataSubmited DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ' AND ' DELIMITED SIZE
                    'CODEPOSTAL ' DELIMITED SIZE
                    '="' DELIMITED SIZE
                   ClientCodePostal of DataSubmited DELIMITED SIZE
                   '"' DELIMITED SIZE

              INTO SQLCA-STATEMENT
           END-STRING.

       Check-If-Client-Exist-Trt.
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
                                              field1

               END-CALL
               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.

       Check-If-Client-Exist-Fin.
           IF SQLCODE EQUAL 0 THEN
               MOVE "ERREUR = UN CLIENT EXISTE DEJA AVEC LES INFORMATION
      -         "S FOURNIES." TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.

           IF SQLCODE NOT EQUAL 0 AND SQLCODE NOT EQUAL 100 THEN
               MOVE "ERREUR = Une erreur SQL non gerer est survenue, veu
      -        "illez voir avec la personne qui gere le programme pour e
      -        "n savoir plus" TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.

           IF SQLCODE EQUAL 100
               perform Insert-Client-If-Not-Exist
           END-IF.

           EXIT.



      ******************************************************************
      *****                INSERT-CLIENT-IF-NOT-EXIST              *****
      ******************************************************************
       Insert-Client-If-Not-Exist.
           perform Insert-Client-If-Not-Exist-Trt.
           perform Insert-Client-If-Not-Exist-Fin.

       Insert-Client-If-Not-Exist-Trt.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'CLIENTS' DELIMITED SIZE
                  '('    DELIMITED SIZE
                  'NOM, ' DELIMITED SIZE
                  'PRENOM, ' DELIMITED SIZE
                  'DATENAISSANCE, '    DELIMITED SIZE
                  'ADRESSE, '    DELIMITED SIZE
                  'CODEPOSTAL, '    DELIMITED SIZE
                  'VILLE, '    DELIMITED SIZE
                  'AGE) '    DELIMITED SIZE
                  'VALUES' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  ClientNom of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  ClientPrenom of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  ClientDateNaissance of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  ClientAdresse of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  ClientCodePostal of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  ClientVille of DataSubmited DELIMITED SIZE
                  '","' DELIMITED SIZE
                  age DELIMITED SIZE
                  '")' DELIMITED SIZE
              INTO SQLCA-STATEMENT
           END-STRING.

           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
           display SQLCODE.
       Insert-Client-If-Not-Exist-Fin.

           IF SQLCODE EQUAL 0 THEN
               MOVE "SUCCES = LE CLIENT A ETE CREER AVEC SUCCES"
               TO MESSAGE-RESPONSE
               MOVE "SUCCES" TO STATUT-RESPONSE
           END-IF.

           IF SQLCODE NOT EQUAL 0 THEN
               MOVE "ERREUR = Une erreur SQL non gerer est survenue, veu
      -        "illez voir avec la personne qui gere le programme pour e
      -        "nsavoir plus" TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.

           EXIT.

      ******************************************************************
      *****                WRITE-RESPONSE-FILE                     *****
      ******************************************************************
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
