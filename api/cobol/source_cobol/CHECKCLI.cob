      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 CHECKCLI.
       AUTHOR.                     Thomas.


      **************************************************************************
      *E N V I R O N M E N T    D I V I S I O N                              *
      **************************************************************************
       ENVIRONMENT DIVISION.
       input-output section.

       file-control.
           select F-DataSubmited
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/check_client_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/check_client_response.txt"
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
         05 ClientId Pic x(6).
         05 SizeOfId Pic x(11).

       01 trash pic X(255).

       01 idSize pic 9.

       01 ClientId-1 pic 9.
       01 ClientId-2 pic 99.
       01 ClientId-3 pic 999.
       01 ClientId-4 pic 9999.

       01 Client.
           05 IdCli pic 9(4).
           05 IdCon pic 9(4).
           05 Nom Pic X(30).
           05 Prenom Pic X(30).
           05 DateNaissance Pic X(15).
           05 Adresse Pic X(100).
           05 CodePostal Pic X(5).
           05 Ville Pic X(30).

       01 MESSAGE-RESPONSE pic X(150).
       01 STATUT-RESPONSE pic X(6).

       01 DATA-RESPONSE pic X(320).
       01 COMPLETE-RESPONSE pic X(1000).

       01 contrat-present pic 9 value 0.


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
       GSPI-Trt.
           perform PGCTB-MAIN.
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
            SizeOfId of champValeur
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

       Generate-Good-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDCLIENT, ' DELIMITED SIZE
                   'CONTRATID, ' DELIMITED SIZE
                   'NOM, ' DELIMITED SIZE
                   'PRENOM, ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'ADRESSE, ' DELIMITED SIZE
                   'CODEPOSTAL, ' DELIMITED SIZE
                   'VILLE '    DELIMITED SIZE
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
                   'IDCLIENT, ' DELIMITED SIZE
                   'CONTRATID, ' DELIMITED SIZE
                   'NOM, ' DELIMITED SIZE
                   'PRENOM, ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'ADRESSE, ' DELIMITED SIZE
                   'CODEPOSTAL, ' DELIMITED SIZE
                   'VILLE '    DELIMITED SIZE
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
                   'IDCLIENT, ' DELIMITED SIZE
                   'CONTRATID, ' DELIMITED SIZE
                   'NOM, ' DELIMITED SIZE
                   'PRENOM, ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'ADRESSE, ' DELIMITED SIZE
                   'CODEPOSTAL, ' DELIMITED SIZE
                   'VILLE '    DELIMITED SIZE
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
                   'IDCLIENT, ' DELIMITED SIZE
                   'CONTRATID, ' DELIMITED SIZE
                   'NOM, ' DELIMITED SIZE
                   'PRENOM, ' DELIMITED SIZE
                   'DATENAISSANCE, ' DELIMITED SIZE
                   'ADRESSE, ' DELIMITED SIZE
                   'CODEPOSTAL, ' DELIMITED SIZE
                   'VILLE '    DELIMITED SIZE
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
           DISPLAY SQLCA-STATEMENT.
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

           MOVE 'CHECKCLI' TO PGCTB-PROGRAM-NAME.

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
      ******************************************************************
      *****                FIN DU FRAMEWORK PGCTBBAT               *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

       PGCTB-ACTION.
           perform Search-Client-With-Id.

       PGCTB-ACTION-FIN.
           perform Write-Response-File.
           EXIT.

      ******************************************************************
      *****               SEARCH-CLIENT-WITH-ID                    *****
      ******************************************************************
       Search-Client-With-Id.
           perform Search-Client-With-Id-Init.
           perform Search-Client-With-Id-Trt.
           perform Search-Client-With-Id-Fin.

       Search-Client-With-Id-Init.
           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).

       Search-Client-With-Id-Trt.
           perform Generate-Good-SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.



           DISPLAY SQLCA-RESULT (1).
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
                                            IdCon of Client
                                            Nom of Client
                                            Prenom of Client
                                            DateNaissance of Client
                                            Adresse of Client
                                            CodePostal of Client
                                            Ville of Client

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
           IF IdCon of Client is numeric and IdCon of Client > 0 then
               move 1 to contrat-present
           ELSE
               move 0 to contrat-present
           END-IF.

       Search-Client-With-Id-Fin.

           IF SQLCODE EQUAL 0 and contrat-present equal 0 THEN
               MOVE "SUCCES = LE CLIENT A BIEN ETE RETROUVER"
               TO MESSAGE-RESPONSE
               MOVE "SUCCES" TO STATUT-RESPONSE
               STRING '{' DELIMITED SIZE
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
                   '}' DELIMITED SIZE
               INTO DATA-RESPONSE
               END-STRING
           END-IF.

           IF SQLCODE EQUAL 0 and contrat-present equal 1 THEN
               MOVE "ERREUR = LE CLIENT A DEJA UN CONTRAT"
               TO MESSAGE-RESPONSE
               MOVE "ERREUR" TO STATUT-RESPONSE

           END-IF

           IF SQLCODE NOT EQUAL 0 THEN
               MOVE "ERREUR = Aucun client ne correspond a ce numero cli
      -        "ent, veuillez verifier le numero renseigné"
                TO MESSAGE-RESPONSE
                MOVE "ERREUR" TO STATUT-RESPONSE
           END-IF.
           EXIT.

       Write-Response-File.
           open output F-Response.
           IF SQLCODE EQUAL 0 and contrat-present equal 0 THEN
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
                   '}' DELIMITED SIZE
           INTO COMPLETE-RESPONSE
           END-STRING
           ELSE 
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
           END-IF.

           write E-Response from COMPLETE-RESPONSE.
           close F-Response.
           EXIT.
