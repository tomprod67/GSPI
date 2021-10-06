<template>

        <div class="wrapperr">
            <div v-if="statusResponseCheckSin == 'SUCCES'">
                <p class="alert alert-success" >{{messageStatus}}</p>

                </div>
                <div v-if="statusResponseCreatSin == 'ERREUR'">
                    <p  class="alert alert-danger">{{messageStatus}}</p>
                </div>
            
            

        <h2>Déclarer un sinistre pour un client</h2>
            <p>Nom du Client : <strong>{{nomCli}}</strong></p>
            <p>Prenom du Client : <strong>{{prenomCli}}</strong></p>
            <p>Age du Client : <strong>{{ageCli}} ans</strong></p>
            <p>Prix par mois actuel du contrat : <strong>{{prixCont}} €</strong></p>
            <form>
                <div>
                    <div class="rowForm">
                        <label for="CH">CH : Chômage</label>
                        <input type="radio" id="CH" name="typeSinistre" v-model="typeSinistre" value="CH">
                    </div>
                     <div class="rowForm">
                        <label for="IA">IA : Invalidité</label>
                        <input type="radio" id="IA" name="typeSinistre" v-model="typeSinistre" value="IA">
                    </div>
                    <div class="rowForm">
                        <label for="PE">PE : Perte d'emploi</label>
                        <input type="radio" id="PE" name="typeSinistre" v-model="typeSinistre" value="PE">
                    </div>
                    <div class="rowForm">
                        <label for="MT">MT : Maternité</label>
                        <input type="radio" id="MT" name="typeSinistre" v-model="typeSinistre" value="MT">
                    </div>
                    <div class="rowForm">
                        <label for="IT">IT : Incapacite temporel de travail</label>
                        <input type="radio" id="IT" name="typeSinistre" v-model="typeSinistre" value="IT">
                    </div>                   
                    <div class="rowForm">
                        <input type="date" v-model="dateSurvenance" name="dateSurvenance" id="dateSurvenance"/>
                        <label for="dateSurvenance">Date de survenance</label>
                    </div>
                    <div class="rowForm">
                        <input type="date" v-model="dateFin" name="dateFin" id="dateFin"/>
                        <label for="dateFin">Date de fin (estimé)</label>
                    </div>
                    <div class="rowForm">
                        <textarea maxlength="200" rows="5" v-model="circonstance" name="circonstance" id="circonstance"></textarea>
                        <label for="circonstance">Circonstance</label>
                    </div>

                </div>
                <button type="button" @click='createSinistre()' class="btn btn-danger">Créer</button>
            </form>
        </div>
</template>

<script>

export default {
  components: {  },
  name: 'CCreateSinistre',
  props: ['statusResponseCreatSin','statusResponseCheckSin','ageCli','prixCont','messageStatus','codeClient', 'codeContrat', 'nomCli', 'prenomCli'],
  data() {
    return {
      typeSinistre: '',
      dateSurvenance: '',
      dateFin: '',
      circonstance: ''

    }
  },
  methods: {
      createSinistre() {
          const payload = {
              typeSinistre: this.typeSinistre,
              dateSurvenance: this.dateSurvenance,
              dateFin: this.dateFin,
              circonstance: this.circonstance,
              idContrat: '',
          }
          this.$emit('createSinistre', payload)
          this.clearForm();
      },
      clearForm() {
          this.typeSinistre = "";
          this.dateSurvenance = "";
          this.dateFin = "";
          this.circonstance = "";
      }
  }
}
</script>