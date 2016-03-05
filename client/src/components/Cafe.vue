<template>
  <div class="ui grid middle align stackable container">
    <div class="one column">
      <div id="pointing" class="ui pointing menu">
        <a id="checkin" class="active item" href="/student/checkin">Check-In</a>
        <a id="attendance" class="item">Attendance</a>
      </div>    
    </div>
  </div>  
  <div class="ui grid middle align stackable container">
    <!-- the UI -->
  </div>  
</template>

<script>
export default {
  data () {
    return {
      // note: changing this line won't causes changes
      // with hot-reload because the reloaded component
      // preserves its current state and we are modifying
      // its initial state.
      msg: 'Cafe Page!'
    }
  }
}
</script>
