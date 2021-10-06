<template>
  <div>
    <CHeader />
   
      <div>
          <div v-if="statusResponseCreatCli != ''">
            <p class="alert alert-danger" v-if="statusResponseCreatCli == 'ERREUR'"> {{messageStatus}} </p>
            <p class="alert alert-success" v-if="statusResponseCreatCli == 'SUCCES'"> La creation du client s'est bien déroulée.</p>
          </div>
          <div v-if="statusResponseCheckCli != ''">
            <p class="alert alert-danger" v-if="statusResponseCheckCli == 'ERREUR'"> {{messageStatus}} </p>
            <p class="alert alert-success" v-if="statusResponseCheckCli == 'SUCCES'"> Les informations correspondent, vous pouvez continuer la création du contrat. </p>
          </div>
          <div v-if="statusResponseCreateCon == 'ERREUR'">
            <p class="alert alert-danger"> {{messageStatus}} </p>
          </div>
          <div v-if="statusResponseCreateCon == 'SUCCES'">
            <p class="alert alert-success"> {{messageStatus}} </p>
          </div>
          <div v-if="statusResponseCheckDos == 'ERREUR'">
            <p class="alert alert-danger">{{messageStatus}}la</p>
          </div>
          <div v-if="statusResponseCreateDos == 'SUCCES'">
            <p class="alert alert-success"> Le Nouveau dossier a bien été créer sur votre contrat et le prix de ce dernier s'est vu augmenterpar la meme occasion afin de couvrir le surplus de garantie. </p>
          </div>   
          <div v-if="statusResponseCreateSin == 'SUCCES'">
            <p> class="alert alert-success" Le Sinistre a bien été enregistré dans le dossier adéquat. Le sinistre est en cours de validation par nos soins. </p>
          </div>               
      </div>
          <div>
            <div id="nav">
              <button type="button" @click='formClient()'>Créer un client</button>  
              <button type="button" @click='formContrat()'>Créer un contrat</button>  
              <button type="button" @click='formDossier()'>Créer un dossier</button>
              <button type="button" @click='formSinistre()'>Déclarer un Sinistre</button>
              <button type="button" @click='prestationShow()'>Liste prestations non validé</button>

            </div>
            <div v-if="formClientActive">
                <CCreateClient @createClient="clientCreate($event)" />
            </div>
            <div v-if="formContratActive">
                <CCheckClient @getCodeClient="checkCodeClient($event)" />
            </div>
            <div v-if="statusResponseCheckCli == 'SUCCES'">
                <CCreateContrat :codeClient="codeClient" @createContrat="contratCreate($event)" />
            </div>
            <div v-if="formDossierActive">
                <CCheckDossier @checkDossier="dossierCheck($event)" />
            </div>
            <div v-if="statusResponseCheckDos == 'SUCCES'">
                <div v-if="d1idDos !== '' && d2idDos == ''">
                    <CCreateDossier :statusResponseCreateDos="statusResponseCreateDos" :messageStatus="messageStatus" :nomCli="nomCli" :prenomCli="prenomCli" :ageCli="ageCli" :codeClient="codeClient" :codeContrat="codeContrat" :d1idDos="d1idDos" :d1type="d1type" :d1montant="d1montant" @createDossier="dossierCreate($event)" />
                </div>
                <div v-if="d2idDos !== '' && d3idDos == ''">
                    <CCreateDossier :statusResponseCreateDos="statusResponseCreateDos" :messageStatus="messageStatus" :nomCli="nomCli" :prenomCli="prenomCli" :ageCli="ageCli" :codeClient="codeClient" :codeContrat="codeContrat" :d1idDos="d1idDos" :d1type="d1type" :d1montant="d1montant" :d2idDos="d2idDos" :d2type="d2type" :d2montant="d2montant" @createDossier="dossierCreate($event)" />
                </div>
                <div v-if="d3idDos !== ''">
                    <CCreateDossier :statusResponseCreateDos="statusResponseCreateDos" :messageStatus="messageStatus" :nomCli="nomCli" :prenomCli="prenomCli" :ageCli="ageCli" :codeClient="codeClient" :codeContrat="codeContrat" :d1idDos="d1idDos" :d1type="d1type" :d1montant="d1montant" :d2idDos="d2idDos" :d2type="d2type" :d2montant="d2montant" :d3idDos="d3idDos" :d3type="d3type" :d3montant="d3montant" @createDossier="dossierCreate($event)" />
                </div>
            </div>


            <div v-if="formSinistreActive">
                <CCheckSinistre :messageStatus="messageStatus" :statusResponseCheckSin="statusResponseCheckSin" @checkSinistre="sinistreCheck($event)" />
            </div>
            <div v-if="statusResponseCheckSin == 'SUCCES'">
               <CCreateSinistre :prixCont="prixCont" :statusResponseCreateSin="statusResponseCreateSin" :statusResponseCheckSin="statusResponseCheckSin" :messageStatus="messageStatus" :nomCli="nomCli" :prenomCli="prenomCli" :ageCli="ageCli" :codeClient="codeClient" :codeContrat="codeContrat" @createSinistre="sinistreCreate($event)" />
            </div>
            <div v-if="pagePrestationActive">
               <CShowPrestation :messageStatus="messageStatus" :arrayPresta="arrayPresta" :statusResponseListPrest="statusResponseListPrest" @showPrestation="prestationShow($event)" />
            </div>
          </div>
  </div>
</template>

<script>
import CCreateClient from './CCreateClient.vue'
import CCreateContrat from './CCreateContrat.vue'
import CCheckClient from './CCheckClient.vue'
import CCheckDossier from './CCheckDossier.vue'
import CCreateDossier from './CCreateDossier.vue'
import CCheckSinistre from './CCheckSinistre.vue'
import CCreateSinistre from './CCreateSinistre.vue'
import CShowPrestation from './CShowPrestation.vue'
import {createClient, getCodeClient} from '../services/ClientService'
import {createContrat} from '../services/ContratService'
import {checkDossier} from '../services/DossierService'
import {createDossier} from '../services/DossierService'
import {checkSinistre} from '../services/SinistreService'
import {createSinistre} from '../services/SinistreService'
import {showPrestation} from '../services/PrestationService'

export default {
  name: 'CMain',
  components: {
    CCreateClient,
    CCheckClient,
    CCreateContrat,
    CCheckDossier,
    CCreateDossier,
    CCheckSinistre,
    CCreateSinistre,
    CShowPrestation,
  },
  data() {
    
      return {
          submitted: false,
          formClientActive: false,
          formContratActive: false,
          formDossierActive: false,
          formSinistreActive: false,
          pagePrestationActive: false,
          customercode: '',
          codeClient:'',
          codeContrat:'',
          nomCli:'',
          prenomCli:'',
          ageCli:'',
          dateSouscriptionCon:'',
          d1idDos: '',
          d2idDos: '',
          d3idDos: '',
          d1type: '',
          d2type: '',
          d3type: '',
          d1montant: '',
          d2montant: '',
          d3montant: '',
          statusResponseCreatCli: '',
          statusResponseCheckCli: '',
          statusResponseCreateCon: '',
          messageStatus: '',
          statusResponseCheckDos: '',
          statusResponseCreateSin:'',
          statusResponseCheckSin:'',
          statusResponseListPrest:'',
          prixCont:'',
          arrayPresta: [],
      }
  },
  methods: {
    clientCreate(data) {
      console.log('data:::', data)
      createClient(data).then(response => {
        console.log(response);
        let dataResponse = JSON.parse(response);
        this.statusResponseCreatCli = dataResponse.statut;
        this.messageStatus = dataResponse.message;
        this.messageStatus = dataResponse.message;
        let dataInresponse = dataResponse.statut;
        this.codeClient = dataInresponse.id;
        this.statusResponseCreatCli = dataResponse.statut;
        this.formClientActive = true;
      });
    },
    contratCreate(data) {
      console.log('data:::', data);
      data.idClient = this.codeClient;
      data.idClient *= 1;
      console.log(data.idClient);
      console.log(data);
      createContrat(data).then(response => {
        let dataResponse = JSON.parse(response);
        this.messageStatus = dataResponse.message;
        console.log(this.messageStatus);
        this.statusResponseCreateCon = dataResponse.statut;
        this.statusResponseCheckCli = '';
        this.codeClient = '';
      });
    },
    checkCodeClient(data) {
      console.log("3");
        console.log('data:::', data)
        getCodeClient(data).then(response => {                  
        let dataResponse = JSON.parse(response);
        this.messageStatus = dataResponse.message;
        this.statusResponseCheckCli = dataResponse.statut;   
        if (this.statusResponseCheckCli == 'SUCCES'){
          this.codeClient = dataResponse.data.id; 
          this.formContratActive = false;
        }
        else this.messageStatus = dataResponse.message;
        console.log(this.codeClient);     

      });
    },
    dossierCheck(data) {
      console.log("3");
        console.log('data:::', data)
        checkDossier(data).then(response => {
          console.log(response);
        let dataResponse = JSON.parse(response);
        this.statusResponseCheckDos = dataResponse.statut;   
        this.messageStatus = dataResponse.message;
        if (this.statusResponseCheckDos == 'SUCCES'){
          this.codeClient = dataResponse.data.client.id; 
          this.codeClient *= 1; 
          this.codeContrat = dataResponse.data.contrat.id; 
          this.codeContrat *= 1; 
          this.nomCli = dataResponse.data.client.nom; 
          this.prenomCli = dataResponse.data.client.prenom; 
          this.ageCli = dataResponse.data.client.age; 
          this.d1idDos = dataResponse.data.dossier_1.idDos; 
          console.log("looool");
          console.log(dataResponse.data);
          if (this.d1idDos !== ''){ 
            this.d1type = dataResponse.data.dossier_1.TypeSinistre; 
            this.d1montant = dataResponse.data.dossier_1.montantGarantie; 
            if (typeof dataResponse.data.dossier_2 !== 'undefined'){
              this.d2idDos =  dataResponse.data.dossier_2.idDos; 
              if (this.d2idDos !== ''){ 
                this.d2type = dataResponse.data.dossier_2.TypeSinistre; 
                this.d2montant = dataResponse.data.dossier_2.montantGarantie; 
                if (typeof dataResponse.data.dossier_3 !== 'undefined'){
                  this.d3idDos =  dataResponse.data.dossier_3.idDos; 
                  if (this.d3idDos !== ''){ 
                    this.d3type = dataResponse.data.dossier_3.TypeSinistre; 
                    this.d3montant = dataResponse.data.dossier_3.montantGarantie; 
                  }
                }
              }
              
            }
          }
        }
        console.log(dataResponse);
        this.formDossierActive = false;
      });
    },
    dossierCreate(data) {
        data.idContrat = this.codeContrat;
        data.idContrat *= 1;
        console.log("3");
        console.log('data:::', data)
        createDossier(data).then(response => {
          let dataResponse = JSON.parse(response);
          this.statusResponseCreateDos = dataResponse.statut;   
          this.messageStatus = dataResponse.message;
          console.log(response)
        if (this.statusResponseCreateDos == 'SUCCES'){
          this.statusResponseCheckDos = ''; 
        }
          
        })
    },
    sinistreCheck(data) {
      console.log('data:::', data)
      checkSinistre(data).then(response => {
            let dataResponse = JSON.parse(response);
            this.statusResponseCheckSin = dataResponse.statut;   
            this.messageStatus = dataResponse.message;
            console.log(response);
            if (this.statusResponseCheckSin == 'SUCCES'){
              this.formSinistreActive = false;
              this.codeClient = dataResponse.data.client.id; 
              this.codeClient *= 1; 
              this.codeContrat = dataResponse.data.contrat.id; 
              this.codeContrat *= 1; 
              this.nomCli = dataResponse.data.client.nom; 
              this.prenomCli = dataResponse.data.client.prenom; 
              this.ageCli = dataResponse.data.client.age; 
              this.prixCont = dataResponse.data.contrat.PrixParMois; 
              console.log(this.statusResponseCheckSin);
   
            }
      });
    },
    sinistreCreate(data) {
        data.idContrat = this.codeContrat;
        data.idContrat *= 1;
        console.log('data:::', data)
        createSinistre(data).then(response => {
          let dataResponse = JSON.parse(response); 
          this.messageStatus = dataResponse.message;
          this.statusResponseCreateSin = dataResponse.statut; 
          this.ageCli = dataResponse.data.client.age; 

          console.log(this.prixCont);
          if (this.statusResponseCreateSin == 'SUCCES'){

            this.statusResponseCheckSin = ''; 
          }


          console.log(this.statusResponseCheckSin)
          
        })
    },    
    prestationShow(data){
        this.pagePrestationActive = true;
        this.formSinistreActive = false;
        this.formContratActive = false;
        this.formClientActive = false;
        this.formDossierActive = false;
        showPrestation(data).then(response => {
          let dataResponse = JSON.parse(response); 
          this.arrayPresta = dataResponse.data.prestation;
          this.messageStatus = dataResponse.message;
          this.statusResponseListPrest = dataResponse.statut; 
          console.log(dataResponse);
        });

      

    },
    formClient() {
        this.formClientActive = true;
        this.formContratActive = false;
        this.formDossierActive = false;
        this.pagePrestationActive = false;
        this.formSinistreActive = false;
    },
    formContrat() {
        this.formContratActive = true;
        this.formClientActive = false;
        this.formDossierActive = false;
        this.pagePrestationActive = false;
        this.formSinistreActive = false;
    },
    formDossier() {
        this.formDossierActive = true;
        this.formClientActive = false;
        this.formContratActive = false;
        this.pagePrestationActive = false;
        this.formSinistreActive = false;
    },
    formSinistre() {
        this.formSinistreActive = true;
        this.formClientActive = false;
        this.formContratActive = false;
        this.pagePrestationActive = false;
        this.formDossierActive = false;

    }
  },
  mounted () {
    
  }
}
</script>