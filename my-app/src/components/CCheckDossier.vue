<template>
        <div class="wrapperr">
        <h2>Création d'un Dossier</h2>
            <form>
                <div>
                    <div class="rowForm">
                        <label for="idClient">Code client</label>
                        <input type="text" v-model="idClient" name="idClient" id="idClient" placeholder="Code Client"/>
                    </div>
                    <div class="rowForm">
                        <label for="idContrat" >Numero de contrat</label>
                        <input type="text" v-model="idContrat" name="idContrat" id="idContrat" placeholder="Numero de contrat"/>
                    </div>
                </div>
                <button type="button" @click='checkDossier()' class="btn btn-danger">Soumettre</button>
            </form>
        </div>
</template>

<script>

export default {
  components: {  },
  name: 'CCheckDossier',
  data() {
    return {
      idClient: "",
      idContrat: ""
    }
  },
  methods: {
      checkDossier() {
          const payload = {
              idClient: this.idClient,
              idContrat: this.idContrat
          }
          console.log(payload);
          this.$emit('checkDossier', payload)
          this.clearForm();
      },
      clearForm() {
          this.idClient = "";
          this.idContrat = "";
      }
  }
}
</script>