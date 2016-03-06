<template>
  <div class="ui grid middle align stackable container">
    <div class="one column">
      <div id="pointing" class="ui pointing menu">
        <a id="checkin" class="active item" v-link="{ path: '/checkin', activeClass: 'active' }">Check-In</a>
        <a id="attendance" class="item" v-link="{ path: '/attendance', activeClass: 'active' }">Attendance</a>
      </div>
    </div>
  </div>
</template>
