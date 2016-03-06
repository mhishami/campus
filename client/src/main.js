import Vue from 'vue'
import App from './components/App.vue'
import Home from './components/Home.vue'
import Checkin from './components/Checkin.vue'
import Cafe from './components/Cafe.vue'
import Attendance from './components/Attendance.vue'

import VueRouter from 'vue-router'
import VueResource from 'vue-resource'

Vue.use(VueResource)
Vue.use(VueRouter)
Vue.use(require('chartist-vuejs'))

export var router = new VueRouter()

router.map({
  '/home': {
    component: Home
  },
  '/checkin': {
    component: Checkin
  },
  '/cafe': {
    component: Cafe
  },
  '/attendance': {
    component: Attendance
  }
})

router.redirect({
  '*': '/home'
})

// Vue.config.debug = true
router.start(App, '#app')
