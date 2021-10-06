<template>
        <div class="wrapperr">
        <p class="alert alert-danger" v-if="statusResponseCheckSin == 'ERREUR'">{{messageStatus}}</p>
        <h2>Déclaration d'un Sinistre</h2>
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
                <button type="button" @click='checkSinistre()' class="btn btn-danger">Soumettre</button>
            </form>
        </div>
</template>

<script>

export default {
  components: {  },
  name: 'CCheckSinistre',
   props: ['statusResponseCheckSin', 'messageStatus'],
  data() {
    return {
      idClient: "",
      idContrat: ""
    }
  },
  methods: {
      checkSinistre() {
          const payload = {
              idClient: this.idClient,
              idContrat: this.idContrat
          }
          console.log(payload);
          this.$emit('checkSinistre', payload)
          this.clearForm();
      },
      clearForm() {
          this.idClient = "";
          this.idContrat = "";
      }
  }
}
</script>