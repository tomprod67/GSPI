const fs = require("fs");
const child_process = require('child_process');


var pgmName = "CREATSIN";
var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
var commande = "rm -f " + finalPgm +";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;



var data_submited = {
        'typeSinistre'  : "JT",
        'dateSurvenance' : "14/07/2021",
        'dateFin'      : "14/07/2021",
        'circonstance'      : "bla bla bla",
        'idContrat' : "1",
        'tailleIdContrat' : "1"
};

for(let key in data_submited){
    data_submited[key] = data_submited[key].trim();
    data_submited[key] = data_submited[key].replace(/ /g, ".");
    }
var ligne_data = "typeSinistre" + ":" + data_submited.typeSinistre + "," +
                 "dateSurvenance" + ":" + data_submited.dateSurvenance + "," +
                 "dateFin"     + ":" + data_submited.dateFin    + "," +
                 "circonstance"     + ":" + data_submited.circonstance + "," +
                 "idContrat"     + ":" + data_submited.idContrat    + "," +
                 "tailleIdContrat"     + ":" + data_submited.tailleIdContrat
try {
    fs.writeFile('data_txt/creation_sinistre_requete.txt', ligne_data, { flag: 'w' }, err => {})
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

/*fs.readFile(__dirname + '/data_txt/creation_sinistre_response.txt', 'utf8' , (err, file) => {
  if (err) {
    console.error(err)
    return
  }
  //REPONSE EN JSON
  var responseJson = JSON.stringify(file);
  console.log(responseJson);
})*/
