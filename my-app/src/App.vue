<template>
  <div id="app">
    <CMain />
  </div>
</template>

<script>
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'

import CMain from './components/CMain.vue'

export default {
  name: 'App',
  components: {
    CMain
  }
}
</script>

<style>
  @import './assets/styles/global.css';
</style>