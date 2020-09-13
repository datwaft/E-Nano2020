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
    run: async function () {
      try {
        var response = await axios.post('/api', {
          data: this.input
        })
        this.output = response.data
      } catch (err) {
        console.error(err)
      }
    },
    cleanInput: function () {
      this.input = ""
    },
    cleanOutput: function () {
      this.output = ""
    },
    getInfo: async function () {
      try {
        var response = await axios.get('/info')
        this.info = response.data
      } catch (err) {
        console.error(err)
      }
    },
    cleanInfo: function () {
      this.info = ""
    },
    highlighter: function (code) {
      return code;
    }
  }
})
