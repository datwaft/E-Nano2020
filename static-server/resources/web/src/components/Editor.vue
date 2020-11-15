<template>
  <div class="Editor">
    <b-container class="editor-container" fluid>
      <b-row class="editor-row">
        <b-col class="fixed-width">
          <codemirror
            v-model="input"
            :options=inputOptions />
        </b-col>
        <b-col class="fixed-width">
          <Result :code="output" />
        </b-col>
      </b-row>
      <b-row>
        <b-col class="input-file-name">
          <b-input-group prepend="Name">
            <b-form-input v-model="namefile"></b-form-input>
          </b-input-group>
        </b-col>
        <b-col>
            <b-button squared variant="light" @click="clearInput">
              <font-awesome-icon icon="broom"/> Clear
            </b-button>
        </b-col>
        <b-col>
          <b-button squared :variant="status" @click="compile" :disabled="disabled">
            <font-awesome-icon :icon="icon" v-if="!disabled"/>
            <b-spinner small v-else></b-spinner>
            Compile
          </b-button>
        </b-col>
        <b-col>
          <b-button squared variant="light" @click="clearOutput">
            <font-awesome-icon icon="broom"/> Clear
          </b-button>
        </b-col>
      </b-row>
      
    </b-container>
    <b-modal
      title="Warning"
      size="sm"
      button-size="sm"
      ok-variant="danger"
      ok-title="Confirm"
      cancel-title="Cancel"
      foorter-class="p-2"
      :hide-header-close="false"
      centered
      v-model="show"
      @ok="clear">
      Do you want to clean the {{ mode }} area?
    </b-modal>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import Result from './Result.vue';
import 'codemirror/mode/clike/clike.js'
import 'codemirror/theme/material.css'

@Component({
  components: {
    Result
  },
})

export default class Editor extends Vue {
  namefile = ""
  input = ""
  output: Array<[string, string]> = []

  status = "success"
  show = false
  mode = ""

  
  inputOptions = {
    tabSize: 2,
    mode: 'text/x-java',
    theme: 'material',
    lineNumbers: true,
    lineWrapping: true
  }
  outputOptions = {
    tabSize: 2,
    mode: 'text',
    theme: 'material',
    lineNumbers: false,
    readOnly: true,
    lineWrapping: true
  }

  get disabled() {
    return this.status === "secondary"
  }
  get icon() {
    switch (this.status) {
      case "danger": return "exclamation-circle"
      case "success": return "paper-plane"
      default: return ""
    }
  }

  async compile() {
    try {
      this.status = "secondary"
      const response = await fetch('http://localhost:8077/compile', {
        method: 'POST',
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          source: this.input,
          name: this.namefile
        })
      })
      const output = (await response.json()).messages
      this.output = JSON.parse(output)
      this.status = "success"
    } catch (err) {
      this.status = "danger"
      console.error(err)
    }
  }
  clearInput() {
    this.mode = "input"
    this.show = true
  }
  clearOutput() {
    this.mode = "output"
    this.show = true
  }
  clear() {
    this.show = false
    setTimeout(() => this.mode = "", 300)
    if (this.mode == "input") this.input = ""
    else this.output = []
  }
}
</script>

<style>

.CodeMirror {
  height: 14rem !important;
  font-size: 1rem !important;
}

.Result pre code {
  font-size: 87.5%;
}

</style>

<style scoped>
.editor-row {
  margin-top: 1%;
  margin-bottom: 1%;
  margin-left: -0.5%;
  margin-right: -0.5%;
}

.fixed-width {
  width: 50%;
  max-width: 50%;
  min-width: 50%;
}

.input-file-name{
  width: 14%;
  max-width: 14%;
  min-width: 14%;
  margin-left: 0.7%;
  margin-right: 3%;
}

</style>