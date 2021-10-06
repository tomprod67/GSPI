<template>
        <div class="wrapperr">
        <h2>Créer un nouveau contrat pour le client n° {{codeClient}}</h2>
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
                <button type="button" @click='createContrat()' class="btn btn-danger">Créer</button>
            </form>
        </div>


</template>

<script>

export default {
  components: {  },
  name: 'CCreateContrat',
  props: ['codeClient'],
  data() {
    return {
      typeSinistre: '',
      montantG: ''   
    }
  },
  methods: {
      createContrat() {
          const payload = {
              typeSinistre: this.typeSinistre,
              montantG: this.montantG,
              idClient: '',
          }
          this.$emit('createContrat', payload)
          this.clearForm();
      },
      clearForm() {
          this.typeSinistre = "";
          this.montantG = "";
      }
  }
}
</script>