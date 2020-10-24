import '@babel/polyfill'
import 'mutationobserver-shim'
import Vue from 'vue'
import './plugins/bootstrap-vue'
import App from './App.vue'
// Code Mirror
import { codemirror } from 'vue-codemirror'
import 'codemirror/lib/codemirror.css'
// FontAwesome
import { library } from '@fortawesome/fontawesome-svg-core'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'
// FontAwesome icons
import { faEtsy } from '@fortawesome/free-brands-svg-icons'
import { faInfoCircle } from '@fortawesome/free-solid-svg-icons'
import { faExclamationCircle } from '@fortawesome/free-solid-svg-icons'
import { faBroom } from '@fortawesome/free-solid-svg-icons'
import { faPaperPlane } from '@fortawesome/free-solid-svg-icons'

library.add(faEtsy)
library.add(faInfoCircle)
library.add(faExclamationCircle)
library.add(faBroom)
library.add(faPaperPlane)

Vue.component('font-awesome-icon', FontAwesomeIcon)
Vue.component('codemirror', codemirror);

Vue.config.productionTip = false

new Vue({
  render: h => h(App),
}).$mount('#app')
