<template>
  <div class="Result CodeMirror">
    <pre><code><div v-for="tuple in code" :key="tuple[1]" :class="tuple[0]">{{ tuple[1] }}</div></code></pre>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';

const ResultProps = Vue.extend({
  props: {
    code: Array
  }
})

@Component
export default class Result extends ResultProps { }
</script>

<style scoped>

.Result {
  padding: 1.25rem 1.5rem;
  background-color: #263238;
  color: #EEFFFF;
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
