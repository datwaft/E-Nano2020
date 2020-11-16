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
            <b-form-input v-model="filename" :class="{ wrong: wrongFileName }" placeholder="File.no"></b-form-input>
          </b-input-group>
        </b-col>
        <b-col>
            <b-button squared variant="light" @click="clearInput">
              <font-awesome-icon icon="broom"/> Clear
            </b-button>
        </b-col>
        <b-col>
          <b-button squared :variant="status" @click="compile" :disabled="disabled || wrongFileName">
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
      <b-row class="editor-row">
        <b-col class="fixed-width-evaluator">
          <Evaluator :history="history" v-model="command" @enter="executeCommand()" />
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
import Evaluator from './Evaluator.vue';
import 'codemirror/mode/clike/clike.js'
import 'codemirror/theme/material.css'

@Component({
  components: {
    Result,
    Evaluator
  },
})

export default class Editor extends Vue {
  filename = "File.no"
  history: Array<object> = []
  command = ""

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

  get wrongFileName() {
    return !/[a-zA-Z_]\w*\.no/.test(this.filename)
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
          filename: this.filename.slice(0, -3)
        })
      })
      const output = (await response.json()).messages
      try {
        this.output = JSON.parse(output)
      } catch {
        this.output = output
      }
      this.status = "success"
    } catch (err) {
      this.status = "danger"
      console.error(err)
    }
  }

  async execute(command: string) {
    try { 
      const response = await fetch('http://localhost:8077/evaluate', {
        method: 'POST',
        headers: {
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          filename: command
        })
      })
      const output = (await response.json()).output 
      this.history.push({
        command: command + ".main()",
        output: {
          type: ( output === "Class not found."  ? "error" : "message"),
          message: output
        }
      })
    } catch (err) {
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

  executeCommand() {
    const matches = /([a-zA-Z_]\w*)\.main\(\)/.exec(this.command)
    if (this.command.trim().length == 0) {
      this.history.push({
        command: "",
        output: {
          type: "message",
          message: ""
        }
      })
    }
    else if (this.command == "clear") {
      this.history = []
    } else if (matches && matches.length > 1) {
      this.execute(matches[1])
    } else if (this.command == "help") {
      this.history.push({
        command: this.command,
        output: {
          type: "success",
          message: "Hello! These are the possible commands:\n" + 
                   "- <filename>.main() : execute the file named <filename> (without the .no termination).\n" + 
                   "  Example: File.main()\n" + 
                   "- clear : clear the evaluation console.\n" + 
                   "- help : show this help message."
        }
      })
    } else {
      this.history.push({
        command: this.command,
        output: {
          type: "error",
          message: "Invalid command."
        }
      })
    }
    this.command = ""
  }
}
</script>

<style>

.CodeMirror {
  height: 35vh !important;
  font-size: 1rem !important;
}

.CodeMirror * {
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
.fixed-width-evaluator {
  width: 100%;
  max-width: 100%;
  min-width: 100%;
}

.input-file-name{
  width: 14%;
  max-width: 14%;
  min-width: 14%;
  margin-left: 0.7%;
  margin-right: 3%;
}
.input-file-name > b-form-input{
  width: 24%;
  max-width: 24%;
  min-width: 24%;
  margin-left: 0.7%;
  margin-right: 3%;
}

.wrong {
  border: 2px solid red;
  border-radius: 4px;
}

</style>
