<template>
  <div class="ui grid">
    <div class="six wide column">
      <div class="ui icon info message" v-show="notice">
        <i class="notched circle loading icon"></i>
        <div class="content">
          <div class="header">
            Touch your card
          </div>
          <p>We're fetching the info for you</p>
        </div>
      </div>
      <div class="ui statistic">
        <div class="value">
          {{ uid }}
        </div>
        <div class="label">
          Card UID
        </div>
      </div>
    </div>
    <div class="ten wide column">
      <user-view></user-view>
    </div>
  </div>
</template>

<script>
import UserView from './UserView.vue'
import Message from './Message.vue'

export default {

  data () {
    return {
      notice: true,
      uid: null
    }
  },

  methods: {
  },

  events: {
    'uid': function (msg) {
      this.uid = msg
      this.notice = false
      this.$broadcast('uid', this.uid)
    }
  },

  components: {
    UserView,
    Message
  }
}

</script>

