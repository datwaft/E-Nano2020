"use strict"

var data = {
  input: "",
  output: "",
  info: ""
}

var app = new Vue({
  el: '#app',
  data: data,
  methods: {
    run: async function (event) {
      try {
        var response = await axios.post('/api', {
          data: this.input
        })
        this.output = response.data
      } catch (err) {
        console.error(err)
      }
    },
    cleanInput: async function (event) {
      this.input = ""
    },
    cleanOutput: async function (event) {
      this.output = ""
    },
    getInfo: async function (event) {
      try {
        var response = await axios.get('/info')
        this.info = response.data
      } catch (err) {
        console.error(err)
      }
    },
    highlighter: function (code) {
      return code;
    }
  }
})
