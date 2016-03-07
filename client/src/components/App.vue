
<template>
  <div class="ui inverted attached fixed menu">
    <div class="ui container">
      <a class="active item" v-link="{ path: '/home', activeClass: 'active' }"><i class="home icon"></i> Home</a>
      <a class="item" v-link="{ path: '/checkin', activeClass: 'active' }">Checkin</a>
      <a class="item" v-link="{ path: '/cafe', activeClass: 'active' }">Cafe</a>
      <a class="item" v-link="{ path: '/reload', activeClass: 'active' }">Reload</a>
      <div class="right menu">
        <button class="ui button inverted" v-link="{ path: '/login', activeClass: 'active' }">Login</button>
      </div>
    </div>
  </div>
  <router-view></router-view>
</template>

<script>
// init out websocket here
const WS_LOC = 'ws://localhost:3000/ws'
const ws = new window.WebSocket(WS_LOC)

export default {

  data () {
    return {
      uid: null
    }
  },

  ready () {
    var self = this
    ws.onmessage = function (evt) {
      if (evt.data !== 'READ') {
        console.log('App: Evt Data: ' + evt.data)
        self.uid = evt.data.split(' ').join('')
        self.$broadcast('uid', self.uid)
      }
    }
  }
}

</script>

<style>
.menu { padding-top: 5px; padding-bottom: 5px; }
.pusher .container { margin-top: 70px; }
</style>

