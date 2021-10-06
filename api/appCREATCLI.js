const fs = require("fs");
const child_process = require('child_process');


var pgmName = "CREATCLI";
var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
var commande = "rm -f " + finalPgm +";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;



var data_submited = {
    'nom'           : "SaintyYY",
    'prenom'        : "Thomasss",
    'dateNaissance' : "15/05/1992     ",
    'adresse'       : "12 rue machin",
    'codePostal'    : "67320",
    'ville'         : "SchilZEigheim"
};

for(let key in data_submited){
    data_submited[key] = data_submited[key].trim();
    data_submited[key] = data_submited[key].replace(/ /g, ".");
    }

var ligne_data = "nom"           + ":" + data_submited.nom           + "," +
                 "prenom"        + ":" + data_submited.prenom        + "," +
                 "dateNaissance" + ":" + data_submited.dateNaissance + "," +
                 "adresse"       + ":" + data_submited.adresse       + "," +
                 "codePostal"    + ":" + data_submited.codePostal    + "," +
                 "ville"         + ":" + data_submited.ville ;

try {
    fs.writeFile('data_txt/creation_client_requete.txt', ligne_data, { flag: 'w' }, err => {})
    //file written successfully
}
catch(err){
    console.log(err)
}
let ls_process = child_process.exec(commande);
var waitTill = new Date(new Date().getTime() + 1 * 2000);
while(waitTill > new Date()){};


let ls_process2 = child_process.spawn(finalPgm);

ls_process2.stdout.on('data', (data) => {
  console.log(`${data}`);
});

fs.readFile(__dirname + '/data_txt/creation_client_response.txt', 'utf8' , (err, file) => {
  if (err) {
    console.error(err)
    return
  }
  //REPONSE EN JSON
  var responseJson = JSON.stringify(file);
  console.log(responseJson);
})
