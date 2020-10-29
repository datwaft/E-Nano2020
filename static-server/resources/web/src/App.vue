<template>
  <div id="app">
    <TitleBar />
    <Editor />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import TitleBar from './components/TitleBar.vue';
import Editor from './components/Editor.vue';

@Component({
  components: {
    TitleBar,
    Editor
  },
})

export default class App extends Vue {}
</script>

<style>
#app {
  min-height: 100vh;
  flex-direction: column;
  color: #2c3e50;
  background-color: #347B9E;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
::-webkit-scrollbar {
  width: 15px;
}

::-webkit-scrollbar-track {
  background: #4a4949;
}

::-webkit-scrollbar-thumb {
  background: rgb(105, 105, 105);
}

::-webkit-scrollbar-thumb:hover {
  background: rgb(173, 173, 173);
}
</style>
