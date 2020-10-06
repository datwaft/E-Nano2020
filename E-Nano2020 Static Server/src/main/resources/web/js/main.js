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

  btn_status: "success",

  info_status: "dark",
  info_show: false,

  mode_confirm: null,
  show_confirm: false
}

let app = new Vue({
  el: '#app',
  data: data,
  computed: {
    btn_disabled: function () {
      return this.btn_status === "secondary";
    },
    btn_class: function () {
      switch (this.btn_status) {
        case "danger": return "fa-exclamation-circle"
        case "success": return "fa-paper-plane"
        default: return ""
      }
    },
    info_disabled: function () {
      return this.info_status === "secondary";
    },
    info_class: function () {
      switch (this.info_status) {
        case "dark": return "fa-info-circle"
        case "danger": return "fa-exclamation-circle"
        default: return ""
      }
    }
  },
  methods: {
    submit: async function () {
      try {
        this.btn_status = "secondary"
        let response = await fetch('http://localhost:8099/api', {
          method: 'POST',
          body: JSON.stringify({
            data: this.input
          })
        })
        this.output = (await response.json()).output
        this.btn_status = "success"
      } catch (err) {
        this.btn_status = "danger"
        console.error(err)
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
      this.show_confirm = false
      setTimeout((() => {
        this.mode_confirm = null
      }), 300)
      this[this.mode_confirm] = ""
    },
    getInfo: async function () {
      try {
        this.info_status = "secondary"
        let response = await fetch('http://localhost:8099/info')
        this.info = await response.json()
        this.info_status = "dark"
        this.info_show = true
        console.log(this.info)
      } catch (err) {
        this.info_status = "danger"
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
