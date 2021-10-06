<template>
  <div>
        <h2>LISTE PRESTATION EN COURS DE TRAITEMENT</h2>
        <table>
          <thead>
            <tr>
            <th>Numéro de Prestation</th>
            <th>Numéro de Sinistre</th>
            <th>Statut de la Prestation</th>
            <th>Action</th>
            </tr>

          </thead>
          <tbody>
            
            <tr @click='detailSinistre()' v-for="item in arrayPresta" :key="item">
                  <td>{{ item.idPresta }}</td>
                  <td>{{ item.idSin }}</td>
                  <td>
                    <span v-if="item.status == 0 || item.status == '0'">
                      En attente de validation
                    </span>
                  </td>
                  <td><a :href="'/api/validPrest/' + item.idPresta">Valider</a></td>                 
            </tr>
          </tbody>
        </table>
    </div>
</template>

<script>

export default {
  components: {  },
  props: ['messageStatus', 'arrayPresta', 'statusResponseListPrest'],
  name: 'CShowPrestation',
  data() {
    return {
      idClient: "",
      idPresta: "",
    }
  },
  methods: {
      showPrestation() {
          const payload = {
              "yo": "yo"
          }
          console.log("dans vue");
          this.$emit('showPrestation', payload)
          document.body.scrollTop = 0;
          document.documentElement.scrollTop = 0;
      },
     
  }
}
</script>