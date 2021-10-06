<template>

        <div class="wrapperr">
            <p class="alert alert-danger" v-if="statusResponseCreateDos == 'ERREUR'">{{messageStatus}}</p>
        <div v-if="d1idDos !== ''">
            <h2>liste des Dossier deja présent sur le contrat n° {{codeContrat}}</h2>
            <ul>
                <li v-if="d1idDos > 0">Dossier n° {{d1idDos}} -> Type de Sinistre : {{d1type}} -> Montant : {{d1montant}}</li>
                <li v-if="d2idDos > 0">Dossier n° {{d2idDos}} -> Type de Sinistre : {{d2type}} -> Montant : {{d2montant}}</li>
                <li v-if="d3idDos > 0">Dossier n° {{d3idDos}} -> Type de Sinistre : {{d3type}} -> Montant : {{d3montant}}</li>

            </ul>
        </div>
        <h2>Créer un nouveau dossier pour le client {{nomCli}} {{prenomCli}}</h2>

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
                        <input type="number" min="100" max="999999" v-model="montantG" name="montantG" id="montantG"/>
                        <label for="montantG">Montant de garantie</label>
                    </div>
 
                </div>
                <button type="button" @click='createDossier()' class="btn btn-danger">Créer</button>
            </form>
        </div>
</template>

<script>

export default {
  components: {  },
  name: 'CCreateContrat',
  props: ['statusResponseCreateDos','messageStatus','codeClient', 'codeContrat', 'nomCli', 'prenomCli', 'd1idDos', 'd1type', 'd1montant','d2idDos', 'd2type', 'd2montant','d3idDos', 'd3type', 'd3montant'],
  data() {
    return {
      typeSinistre: '',
      montantG: ''   
    }
  },
  methods: {
      createDossier() {
          const payload = {
              typeSinistre: this.typeSinistre,
              montantG: this.montantG,
              idContrat: '',
          }
          this.$emit('createDossier', payload)
          this.clearForm();
      },
      clearForm() {
          this.typeSinistre = "";
          this.montantG = "";
      }
  }
}
</script>