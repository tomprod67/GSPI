const fs = require("fs");
const child_process = require('child_process');


var pgmName = "DETAILSI";
var finalPgm = __dirname + "/cobol/programme_cobol/" + pgmName;
var pgm = __dirname + "/cobol/source_cobol/" + pgmName + ".cob ";
var api = __dirname + "/cobol/dbpre/cobmysqlapi.o";
var commande = "rm -f " + finalPgm +";cobc -x " + pgm + api + " -L/usr/lib/mysql -lmysqlclient -o" + finalPgm;



var data_submited = {
        'idSin' : "5",
        'tailleIdSin' : "1"
};

for(let key in data_submited){
    data_submited[key] = data_submited[key].trim();
    data_submited[key] = data_submited[key].replace(/ /g, ".");
    }
var ligne_data = "idSin" + ":" + data_submited.idSin + "," +
                 "tailleIdSin" + ":" + data_submited.tailleIdSin
try {
    fs.writeFile('data_txt/detail_sinistre_requete.txt', ligne_data, { flag: 'w' }, err => {})
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

/*fs.readFile(__dirname + '/data_txt/detail_sinistre_response.txt', 'utf8' , (err, file) => {
  if (err) {
    console.error(err)
    return
  }
  //REPONSE EN JSON
  var responseJson = JSON.stringify(file);
  console.log(responseJson);
})*/
