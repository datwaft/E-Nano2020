<template>
  <div class="TitleBar">
    <b-navbar type="light" variant="dark" sticky>
      <b-navbar-brand class="brand">
        <font-awesome-icon :icon="['fab', 'etsy']" />-Nano2020
      </b-navbar-brand>
      <b-navbar-nav class="ml-auto">
        <b-nav-form>
          <b-button @click="getInfo" :variant="status" :disabled="disabled">
            <font-awesome-icon :icon="icon" v-if="!disabled" />
            <b-spinner small v-else></b-spinner>
            About
          </b-button>
        </b-nav-form>
      </b-navbar-nav>
    </b-navbar>
    <b-sidebar
      bg-variant="dark"
      text-variant="light"
      title="About"
      v-model="show"
      right shadow>
      <div class="px-3 py-2">
        <template v-if="info">
          <h5>Project</h5> {{ info.project }}
          <h5>Course</h5> {{ info.course }}
          <h5>Instance</h5> {{ info.instance }}
          <h5>Cycle</h5> {{ info.cycle }}
          <h5>Organization</h5> {{ info.organization }}
          <h5>Project site</h5> <a :href="info.projectSite"> {{ info.projectSite }}</a>
          <h5>Team</h5>
          <b>Code:</b> {{ info.team.code }}
          <br>
          <b>Members:</b>
          <ul>
            <li v-for="member in info.team.members" :key="member.id">
              {{ member.firstName }} {{ member.surnames }} <br>
              {{ member.id }}
            </li>
          </ul>
        </template>
      </div>
    </b-sidebar>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';

@Component
export default class TitleBar extends Vue {
  info = ""

  status = "dark"
  show = false

  get disabled() {
    return this.status === "secondary"
  }
  get icon() {
    switch (this.status) {
      case "dark": return "info-circle"
      case "danger": return "exclamation-circle"
      default: return ""
    }
  }

  async getInfo() {
    try {
      this.status = "secondary"
      const response = await fetch('http://localhost:8099/info')
      this.info = await response.json()
      this.status = "dark"
      this.show = true
    } catch (err) {
      this.status = "danger"
      console.error(err)
    }
  }
  clearInfo() {
    this.info = ""
  }
}
</script>

<style scoped>
.brand {
  font-size: 2rem;
  color: white;
  cursor: pointer;
  user-select: none;
}
.brand:hover {
  font-size: 2rem;
  color: white;
}
</style>
