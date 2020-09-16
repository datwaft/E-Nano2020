/**
 *  _____      _   _                  ____   ___ ____   ___
 * | ____|    | \ | | __ _ _ __   ___|___ \ / _ \___ \ / _ \
 * |  _| _____|  \| |/ _` | '_ \ / _ \ __) | | | |__) | | | |
 * | |__|_____| |\  | (_| | | | | (_) / __/| |_| / __/| |_| |
 * |_____|    |_| \_|\__,_|_| |_|\___/_____|\___/_____|\___/
 *
 * File: main.js
 * Description:
 *    Archivo inicial del cliente utilizando Vue.js.
 * Authors:
 * - David Alberto Guevara Sánchez
 *   402450355
 * - Joy Bonilla Fley
 *   402360421
 * - Jose Barrantes Araya
 *   207600954
 * - Natalia Solano Azofeifa
 *   117290958
 * - Luis David Villalobos Gonzalez
 *   117540697
 * Group: 03
 * Schedule: 10am
 * Date of modification: 2020-09-16
 */

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
    submit: async function () {
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
