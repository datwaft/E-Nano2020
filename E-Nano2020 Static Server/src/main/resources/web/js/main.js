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
 * Date of modification: 2020-10-04
 */

"use strict"

let data = {
  input: "",
  output: "",
  info: "",

  btn_disabled: false,

  mode_confirm: null,
  show_confirm: false
}

let app = new Vue({
  el: '#app',
  data: data,
  methods: {
    submit: async function () {
      try {
        this.btn_disabled = true
        let response = await fetch('http://localhost:8099/api', {
          method: 'POST',
          body: JSON.stringify({
            data: this.input
          })
        })
        this.output = (await response.json()).output
        this.btn_disabled = false
      } catch (err) {
        console.error(err)
        this.btn_disabled = false
      }
    },
    cleanInput: function () {
      this.mode_confirm = "input"
      this.show_confirm = true
    },
    cleanOutput: function () {
      this.mode_confirm = "output"
      this.show_confirm = true
    },
    clean: function () {
      this[this.mode_confirm] = ""
      this.mode_confirm = null
      this.show_confirm = false
    },
    getInfo: async function () {
      try {
        let response = await fetch('http://localhost:8099/info')
        this.info = await response.json()
        console.log(this.info)
      } catch (err) {
        console.error(err)
      }
    },
    cleanInfo: function () {
      this.info = ""
    },
    highlightInput: function (code) {
      return Prism.highlight(code, Prism.languages.java, "java");
    },
    highlightOutput: function (code) {
      return code;
    }
  }
})
