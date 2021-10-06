<template>
        <div class="wrapperr">
        <h2>Création d'un Contrat</h2>
            <form>
                <div>
                    <div class="rowForm">
                        <label htmlFor="exampleInputEmail1">Code client</label>
                        <input type="text" class="form-control" v-model="idClient" name="idClient" id="idClient" aria-describedby="emailHelp" placeholder="Code Client"/>
                    </div>
                </div>
                <button type="button" @click='getCodeClient()' class="btn btn-danger">Soumettre</button>
            </form>
        </div>
</template>

<script>

export default {
  components: {  },
  name: 'CCheckClient',
  data() {
    return {
      idClient: ""
    }
  },
  methods: {
      getCodeClient() {
          const payload = {
              idClient: this.idClient
          }
          console.log("1");
          this.$emit('getCodeClient', payload)
          this.clearForm();
          document.body.scrollTop = 0;
          document.documentElement.scrollTop = 0;
      },
      clearForm() {
          this.customercode = "";
      }
  }
}
</script>