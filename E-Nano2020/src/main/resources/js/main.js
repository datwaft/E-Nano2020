"use strict"

var data = {
  input: "",
  output: ""
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
        console.log(response.data)
      } catch (err) {
        console.error(err)
      }
    },
    highlighter: function (code) {
      return code;
    }
  }
})