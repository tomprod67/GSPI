      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 DETAILSI.
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
           assign to "/home/thomas/dev/projet_GSPI/data_txt/detail_sinis
      -              "tre_requete.txt"
           organization is line sequential.

           select F-Response
           assign to "/home/thomas/dev/projet_GSPI/data_txt/detail_sinis
      -              "tre_response.txt"
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
         05 SinistreId Pic x(17).
         05 SizeOfId Pic x(16).

       01 trash pic X(255).

       01 idSize pic 9.

       01 SinistreId-1 pic 9.
       01 SinistreId-2 pic 99.
       01 SinistreId-3 pic 999.
       01 SinistreId-4 pic 9999.

       01 Sin.
         05 IdSini pic 9(4).
         05 TypeSin pic X(2).
         05 StatusSin pic 9.
         05 DateDeclaration pic X(10).
         05 DateSurvenance pic X(10).
         05 DateFin pic X(10).
         05 Circonstance pic X(200).



       01 MESSAGE-RESPONSE pic X(150).
       01 STATUT-RESPONSE pic X(6).

       01 COMPLETE-RESPONSE pic X(1000).
       01 SINISTRE-STRING pic X(600).


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
           perform Select-Sinistre.
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
            SinistreId of champValeur
            SizeOfId of champValeur
           end-unstring.

           unstring SizeOfId of champValeur delimited by ":" into
            trash
            idSize
           end-unstring.
           display SizeOfId of champValeur.
           EVALUATE idSize
             WHEN 1
             display "la1"
               unstring SinistreId of champValeur delimited by ":" into
               trash
               SinistreId-1
               end-unstring
               SET id1-bool TO TRUE
             WHEN 2
             display "la2"
               unstring SinistreId of champValeur delimited by ":" into
               trash
               SinistreId-2
               end-unstring
               SET id2-bool TO TRUE
             WHEN 3
             display "la3"
               unstring SinistreId of champValeur delimited by ":" into
               trash
               SinistreId-3
               end-unstring
               SET id3-bool TO TRUE
             WHEN 4
               unstring SinistreId of champValeur delimited by ":" into
               trash
               SinistreId-4
               end-unstring
               SET id4-bool TO TRUE
           end-evaluate.


       Unstring-Line-Fin.
           EXIT.


       Select-Sinistre.
           perform Select-Sinistre-Init.
           perform Select-Sinistre-Trt.
           perform Select-Sinistre-Fin.

       Select-Sinistre-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.
           MOVE 0 TO SQLCODE.

           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).


       Select-Sinistre-Trt.
           perform Generate-SelectSin-SQLCA-STATEMENT.
           display SQLCA-STATEMENT.
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
                                            IdSini of Sin
                                            TypeSin of Sin
                                            StatusSin of Sin
                                            DateDeclaration of Sin
                                            DateSurvenance of Sin
                                            DateFin of Sin
                                            Circonstance of Sin

               END-CALL

               IF SQLCA-RESULT (1) = NULL
                   MOVE 100 TO SQLCODE
               ELSE
                   MOVE 0 TO SQLCODE
               END-IF
           END-IF.
               display sqlcode.
           if SQLCODE equal 0 then
           MOVE "SUCCES = VOILA LA LISTE"
           TO MESSAGE-RESPONSE
           MOVE "SUCCES" TO STATUT-RESPONSE
           STRING      '{' DELIMITED SIZE
                       '"id" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       IdSini of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"typeSinistre" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       TypeSin of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"status" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       StatusSin of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"dateDeclaration" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       DateDeclaration of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"dateSurvenance" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       DateSurvenance of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"dateFin" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       DateFin of Sin DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"circonstance" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       Circonstance of Sin DELIMITED SIZE
                       '"}' DELIMITED SIZE
                   INTO SINISTRE-STRING
               END-STRING
           end-if.
           if SQLCODE is not equal 0 then
               MOVE "ERROR = UNE ERREUR SQL NON GEREE EST SURVENUE."
               TO MESSAGE-RESPONSE
               MOVE "ERROR" TO STATUT-RESPONSE
           end-if.

       Select-Sinistre-Fin.
           perform close-BDD.
           EXIT.



       Write-Response-File.
           open output F-Response.
           IF SQLCODE equal 0 then
               STRING '{' DELIMITED SIZE
                       '"statut" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       STATUT-RESPONSE DELIMITED SIZE
                       '"' DELIMITED SIZE
                       ',' DELIMITED SIZE
                       '"message" : ' DELIMITED SIZE
                       '"' DELIMITED SIZE
                       MESSAGE-RESPONSE DELIMITED SIZE
                       '",' DELIMITED SIZE
                       '"sinistre" : ' DELIMITED SIZE
                       SINISTRE-STRING DELIMITED SIZE
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
                       '"}' DELIMITED SIZE
               INTO COMPLETE-RESPONSE
               END-STRING
           END-IF.
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
       Generate-SelectSin-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           evaluate TRUE
           when id1-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDSINISTRE, ' DELIMITED SIZE
                   'TYPESINISTRE, ' DELIMITED SIZE
                   'STATUS, ' DELIMITED SIZE
                   'DATEDECLARATION, ' DELIMITED SIZE
                   'DATESURVENANCE, ' DELIMITED SIZE
                   'DATEFIN, ' DELIMITED SIZE
                   'CIRCONSTANCE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'SINISTRES '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDSINISTRE '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   SinistreId-1 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id2-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDSINISTRE, ' DELIMITED SIZE
                   'TYPESINISTRE, ' DELIMITED SIZE
                   'STATUS, ' DELIMITED SIZE
                   'DATEDECLARATION, ' DELIMITED SIZE
                   'DATESURVENANCE, ' DELIMITED SIZE
                   'DATEFIN, ' DELIMITED SIZE
                   'CIRCONSTANCE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'SINISTRES '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDSINISTRE '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   SinistreId-2 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id3-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDSINISTRE, ' DELIMITED SIZE
                   'TYPESINISTRE, ' DELIMITED SIZE
                   'STATUS, ' DELIMITED SIZE
                   'DATEDECLARATION, ' DELIMITED SIZE
                   'DATESURVENANCE, ' DELIMITED SIZE
                   'DATEFIN, ' DELIMITED SIZE
                   'CIRCONSTANCE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'SINISTRES '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDSINISTRE '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   SinistreId-3 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           when id4-bool
               STRING 'SELECT ' DELIMITED SIZE
                   'IDSINISTRE, ' DELIMITED SIZE
                   'TYPESINISTRE, ' DELIMITED SIZE
                   'STATUS, ' DELIMITED SIZE
                   'DATEDECLARATION, ' DELIMITED SIZE
                   'DATESURVENANCE, ' DELIMITED SIZE
                   'DATEFIN, ' DELIMITED SIZE
                   'CIRCONSTANCE ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'SINISTRES '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'IDSINISTRE '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   SinistreId-4 DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING
           end-evaluate.
       Generate-SelectSin-SQLCA-STATEMENT-Fin.
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
           MOVE 'DETAILSI' TO PGCTB-PROGRAM-NAME.

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
