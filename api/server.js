const express = require('express');
const path = require('path');
const randomId = require('random-id');
const app = express(),
    bodyParser = require("body-parser");
port = 3080;
const fs = require("fs");
const child_process = require('child_process');
const { json } = require('express');



// place holder for the data
const users = [];

// codeClient en dur
const codeclient = "0000";

app.use(bodyParser.json());
app.use(express.static(path.join(__dirname, '../my-app/dist')));

/********************************************************************************
 * ******************************************************************************
 *****                           CREATE CLIENT                              *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/client', (req, res) => {
    const client = req.body.client;
    var pgmName = "CREATCLI";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;

    for (let key in client) {
        client[key] = client[key].trim();
        client[key] = client[key].replace(/ /g, ".");
    }

    var ligne_data = "nom" + ":" + client.lastName + "," +
        "prenom" + ":" + client.firstname + "," +
        "dateNaissance" + ":" + client.date + "," +
        "adresse" + ":" + client.zipcode + "," +
        "codePostal" + ":" + client.address + "," +
        "ville" + ":" + client.city;

    console.log(ligne_data);
    try {
        fs.writeFile('data_txt/creation_client_requete.txt', ligne_data, { flag: 'w' }, err => {})
        let ls_process = child_process.exec(commande);
        var waitTill = new Date(new Date().getTime() + 1 * 2000);
        while (waitTill > new Date()) {};
        var file;

        let ls_process2 = child_process.spawn(finalPgm);

        fs.readFile(__dirname + '/data_txt/creation_client_response.txt', 'utf8', (err, file) => {
            if (err) {
                console.error(err)
                return
            }
            res.json(file);
        })
    } catch (err) {
        console.log(err)
    }

});

/********************************************************************************
 * ******************************************************************************
 *****                         CREATE CONTRAT                               *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/contrat', (req, res) => {
    const contrat = req.body.contrat;
    var pgmName = "CREATCON";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    console.log(contrat);

    var taille_id = contrat.idClient.toString().length;
    console.log(taille_id);

    var ligne_data = "typeSinistre" + ":" + contrat.typeSinistre + "," +
        "montantG" + ":" + contrat.montantG + "," +
        "idClient" + ":" + contrat.idClient + "," +
        "taille_id" + ":" + taille_id;
    console.log('jesuisici');
    console.log(ligne_data);
    try {
        fs.writeFile('data_txt/creation_contrat_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/creation_contrat_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }
        res.json(file);
    })

});

/********************************************************************************
 * ******************************************************************************
 *****                   CHECK CLIENT FOR CREATE CONTRAT                   *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/getcodeclient', (req, res) => {
    const customercode = req.body.idClient;
    console.log(customercode);
    var pgmName = "CHECKCLI";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    var taille_id = customercode.idClient.length;
    var idClient = customercode.idClient.trim();


    var ligne_data = "id" + ":" + idClient + "," +
        "taille_id" + ":" + taille_id;
    try {
        fs.writeFile('data_txt/check_client_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/check_client_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }
        console.log(file.trim());
        res.json(file);
    })
});


/********************************************************************************
 * ******************************************************************************
 *****                  CHECK DOSSIER FOR CREATE DOSSIER                    *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/checkDossier', (req, res) => {
    var dossier = req.body.dossier;

    var pgmName = "CHECKDOS";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    var taille_id_cli = dossier.idClient.toString().length;
    var taille_id_con = dossier.idContrat.toString().length;
    var ligne_data = "idClient" + ":" + dossier.idClient + "," +
        "tailleIdClient" + ":" + taille_id_cli + "," +
        "idContrat" + ":" + dossier.idContrat + "," +
        "tailleIdContrat" + ":" + taille_id_con;
    try {
        fs.writeFile('data_txt/check_dossier_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/check_dossier_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }

        res.json(file);
    })
});

/********************************************************************************
 * ******************************************************************************
 *****                       CREATE DOSSIER                                 *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/createDossier', (req, res) => {
    var dossier = req.body.dossier;

    var pgmName = "CREATDOS";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    console.log("icicici");
    var taille_id_con = dossier.idContrat.toString().length;

    var ligne_data = "typeSinistre" + ":" + dossier.typeSinistre + "," +
        "montantGarantie" + ":" + dossier.montantG + "," +
        "idContrat" + ":" + dossier.idContrat + "," +
        "taille_id" + ":" + taille_id_con;
    try {
        fs.writeFile('data_txt/creation_dossier_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/creation_dossier_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }

        res.json(file);
    })

});

/********************************************************************************
 * ******************************************************************************
 *****                  CHECK SINISTRE FOR CREATE SINISTRE                  *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/checkSinistre', (req, res) => {
    var sinistre = req.body.sinistre;

    var pgmName = "CHECKSIN";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    var taille_id_cli = sinistre.idClient.toString().length;
    var taille_id_con = sinistre.idContrat.toString().length;
    var ligne_data = "idClient" + ":" + sinistre.idClient + "," +
        "tailleIdClient" + ":" + taille_id_cli + "," +
        "idContrat" + ":" + sinistre.idContrat + "," +
        "tailleIdContrat" + ":" + taille_id_con;
    try {
        fs.writeFile('data_txt/check_sinistre_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/check_sinistre_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }

        res.json(file);
    })
});
/********************************************************************************
 * ******************************************************************************
 *****                       CREATE SINISTRE                                *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/createSinistre', (req, res) => {
    var sinistre = req.body.sinistre;

    var pgmName = "CREATSIN";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;
    console.log("icicici");
    var taille_id_con = sinistre.idContrat.toString().length;
    let dateSurv = sinistre.dateSurvenance.split('-');
    let dateFin = sinistre.dateFin.split('-');
    var dateFinFormated = dateFin[2] + "/" + dateFin[1] + "/" + dateFin[0];
    var dateSurvFormated = dateSurv[2] + "/" + dateSurv[1] + "/" + dateSurv[0];

    var ligne_data = "typeSinistre" + ":" + sinistre.typeSinistre + "," +
        "dateSurvenance" + ":" + dateSurvFormated + "," +
        "dateFin" + ":" + dateFinFormated + "," +
        "circonstance" + ":" + sinistre.circonstance + "," +
        "idContrat" + ":" + sinistre.idContrat + "," +
        "tailleIdContrat" + ":" + taille_id_con;

    try {
        fs.writeFile('data_txt/create_sinistre_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};
    var file;

    let ls_process2 = child_process.spawn(finalPgm);

    fs.readFile(__dirname + '/data_txt/create_sinistre_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }

        res.json(file);
    })


});

/********************************************************************************
 * ******************************************************************************
 *****                       GET PRESTATION                                 *****
 * ******************************************************************************
 ********************************************************************************/
app.post('/api/showPrestation', (req, res) => {
    var prestation = req.body.prestation;
    console.log(prestation);

    var pgmName = "LISTPRES";
    var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
    var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
    var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
    var commande = "rm -f " + finalPgm + ";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;


    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};

    let ls_process2 = child_process.spawn(finalPgm);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};

    fs.readFile(__dirname + '/data_txt/liste_prestation_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }
        console.log("dans serveur");
        console.log(Date.now());
        res.json(file);
    })



});
app.post('/api/validPrest/:idPrest', (req, res) => {
    var idPrestTemp = req.params.idPrest;
    var idPrest = idPrestTemp * 1;
    var taille_id = idPrest.toString().length;
    console.log(idPrest);
    var ligne_data = "idPrest" + ":" + idPrest + "," +
        "tailleIdPrest" + ":" + taille_id
    try {
        fs.writeFile('data_txt/valide_prestation_requete.txt', ligne_data, { flag: 'w' }, err => {})
            //file written successfully
    } catch (err) {
        console.log(err)
    }
    let ls_process = child_process.exec(commande);
    var waitTill = new Date(new Date().getTime() + 1 * 2000);
    while (waitTill > new Date()) {};


    let ls_process2 = child_process.spawn(finalPgm);
    fs.readFile(__dirname + '/data_txt/valide_prestation_response.txt', 'utf8', (err, file) => {
        if (err) {
            console.error(err)
            return
        }
        //REPONSE EN JSON
        var responseJson = JSON.stringify(file);
        console.log(responseJson);
    })
    res.json("validprest");

});

app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, '../my-app/build/index.html'));
});

app.listen(port, () => {
    console.log(`Server listening on the port::${port}`);
});