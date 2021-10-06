<template>
        <div class="wrapperr">
        <h2>Créer un nouveau client</h2>
            <form>
                <div>
                    <div class=rowForm>
                        <label htmlFor="exampleInputEmail1">Nom</label>
                        <input type="text" class="form-control" v-model="lastName" name="lastname" id="lastname" aria-describedby="emailHelp" placeholder="Nom" />
                    </div>
                    <div class="rowForm">
                        <label htmlFor="exampleInputPassword1">Prénom</label>
                        <input type="text" class="form-control" v-model="firstname" name="firstname" id="firstname" placeholder="Prénom" />
                    </div>
                </div>
                <div>
                    <div class="rowForm">
                        <label htmlFor="exampleInputEmail1">Date de naissance</label>
                        <input type="text" class="form-control" v-model="date" name="date" id="date" aria-describedby="emailHelp" placeholder="Date de naissance" />
                    </div>
                </div>
                <div>
                    <div class="rowForm">
                        <label htmlFor="exampleInputEmail1">Code Postal</label>
                        <input type="text" class="form-control" v-model="zipcode" name="zipcode" id="zipcode" aria-describedby="emailHelp" placeholder="Code postal" />
                    </div>
                </div>
                <div>
                    <div class="rowForm">
                        <label for="address">Adresse</label>
                        <textarea id="address" v-model="address" 
                            name="address" placeholder="Adresse">
                        </textarea>
                    </div>
                </div>
                <div class="row">
                    <div class="rowForm">
                        <label htmlFor="exampleInputEmail1">Ville</label>
                        <input type="text" class="form-control" v-model="city" name="city" id="city" aria-describedby="emailHelp" placeholder="Ville" />
                    </div>
                </div>    
                <button type="button" @click='createClient()' class="btn btn-danger">Créer</button>
            </form>
        </div>

</template>

<script>

export default {
  components: {  },
  name: 'CCreateClient',
  data() {
    return {
      lastName: '',
      firstname: '',
      date: '',
      zipcode: '',
      address: '',
      city: '' 
    }
  },
  methods: {
      createClient() {
          console.log(this.firstname)
          const payload = {
              lastName: this.lastName,
              firstname: this.firstname,
              date: this.date,
              zipcode: this.zipcode,
              address: this.address,
              city: this.city
          }
          this.$emit('createClient', payload)
          this.clearForm();
      },
      clearForm() {
          this.firstname = "";
          this.lastName = "";
          this.date = "";
          this.zipcode = "";
          this.address = "";
          this.city = "";
      }
  }
}
</script>