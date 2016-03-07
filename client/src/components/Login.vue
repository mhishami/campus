<template>
  <div class="pusher">
    <div class="ui grid middle align stackable container">
      <div class="row">
        <div class="five wide column">
          <!-- <pre>{{ $data | json }}</pre> -->
        </div>
        <div id="login" class="six wide column ui segment">
          <form class="ui form">
            <h2 class="ui dividing header"><i class="key icon"></i> Login</h2>
            <div class="field">
              <label>Username</label>
              <input type="text" name="username" v-model="credentials.username" placeholder="Username" />
            </div>
            <div class="field">
              <label>Password</label>
              <input type="password" name="password" v-model="credentials.password" placeholder="Password" />
            </div>
            <div class="field">
              <div class="ui checkbox">
                <input type="checkbox" tabindex="0" v-model="agree" class="hidden" />
                <label>I agree to the Terms and Conditions</label>
              </div>
            </div>
            <button class="ui button primary" v-on:click="login()" v-bind:disabled="is_invalid">Login</button>
          </form>
        </div>
        <div class="five wide column">
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import api from '../api'

export default {
  ready () {

  },

  data () {
    return {
      agree: true,
      credentials: {
        username: '',
        password: ''
      }
    }
  },

  computed: {
    'is_invalid': function () {
      if (this.credentials.username !== '' &&
          this.credentials.password !== '') {
        return false
      }
      return true
    }
  },

  events: {

  },

  methods: {
    login () {
      api.authenticate(this, this.credentials)
    }
  }
}
</script>

<style>
#login { margin-top: 100px; }
</style>
