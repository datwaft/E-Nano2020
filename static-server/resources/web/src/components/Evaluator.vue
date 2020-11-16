<template>
  <div class="Evaluator">
    <div v-for="(element, index) in history" :key="index">
      <span class="delimiter">{{ command_delimiter }}</span>
      <span>{{ element.command }}</span>
      <br>
      <span :class="element.output.type">{{ element.output.message }}</span>
    </div>
    <div class="command">
      <span class="delimiter">{{ command_delimiter }}</span>
      <span>
        <input
          type="text"
          :value="command"
          @change="$emit('change', $event.target.value)"
          @keyup.enter="$emit('enter')">
      </span>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Model } from 'vue-property-decorator';

const EvaluatorProps = Vue.extend({
  props: {
    history: Array,
    command: String
  }
})

@Component
export default class Evaluator extends EvaluatorProps {
  @Model('change', { type: String }) readonly command!: string
  command_delimiter = "$"
}
</script>

<style scoped>

.Evaluator {
  background-color: #263238;
  color: #EEFFFF;
  font-family: SFMono-Regular,Menlo,Monaco,Consolas,"Liberation Mono","Courier New",monospace;
  padding: 1rem;
  height: 35vh;
  overflow-y: scroll;
  white-space: pre;
}

input {
  background-color: #263238;
  color: #EEFFFF;
  outline: none;
  border: 0;
}

.delimiter {
  padding-right: 1rem;
  color: #EEEEEE;
  user-select: none;
}

.command {
  display: flex;
}

.command span:not(.delimiter) {
  display: flex;
  flex-grow: 1;
}

.command input {
  flex-grow: 1;
}

.error {
  color: #e2777a;
}

.warning {
  color: #f8c555;
}

.success {
  color: #7ec699;
}

.message {
  color: #EEFFFF;
}

</style>

