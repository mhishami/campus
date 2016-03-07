<template>
<div class="pusher">
  <checkin-menu></checkin-menu>
  <div class="ui grid middle align stackable container">
    <h2 class="ui header">
      <i class="student icon"></i>
      <div class="content">
        Attendance
      </div>
    </h2>
    <line-chart></line-chart>
  </div>
</div>
</template>

<script>
import CheckinMenu from './CheckinMenu.vue'
import LineChart from './LineChart.vue'

export default {
  ready () {
  },

  components: {
    CheckinMenu,
    LineChart
  }
}

</script>