import Vue from 'vue'
import App from './components/App.vue'
import Home from './components/Home.vue'
import Checkin from './components/Checkin.vue'
import Cafe from './components/Cafe.vue'

import VueRouter from 'vue-router'
import VueResource from 'vue-resource'
Vue.use(VueResource)
Vue.use(VueRouter)

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
  }
})

router.redirect({
  '*': '/home'
})

router.start(App, '#app')
