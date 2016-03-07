<template>
  <div class="ui grid middle align stackable container">
    <div class="ui ordered steps">
      <div class="active step">
        <div class="content">
          <div class="title">Check Balance</div>
          <div class="description">Get current balance</div>
        </div>
      </div>
      <div class="step">
        <div class="content">
          <div class="title">Reload</div>
          <div class="description">Reload card</div>
        </div>
      </div>
    </div>
  </div>
  <div class="ui grid middle align stackable container">
    <!-- make even column -->
    <div class="eight wide column">
      <div class="ui icon info message" v-show="notice">
        <i class="notched circle loading icon"></i>
        <div class="content">
          <div class="header">
            Touch your card for Reload
          </div>
          <p>We're fetching the current balance for you</p>
        </div>
      </div>
<!--
      <div class="ui segment">
        <pre>{{ $data | json }}</pre>
      </div>
 -->
    </div>

    <div class="eight wide column">
      <div class="ui form segment">
        <div class="two fields">
          <div class="field">
            <label>Current Balance</label>
            <input value="{{ user.balances | currency 'RM ' }}" readonly=""></input>
          </div>
          <div class="field">
            <label>Card ID</label>
            <input value="{{ user.card_uid }}" readonly=""></input>
          </div>
        </div>
        <div class="two fields">
          <div class="field">
            <label>First Name</label>
            <input value="{{ user.first }}" readonly=""></input>
          </div>
          <div class="field">
            <label>Last Name</label>
            <input value="{{ user.last }}" readonly=""></input>
          </div>
        </div>
        <div class="ui divider"></div>
        <div class="one field">
          <div class="field">
            <label>Reload Amount</label>
            <div class="ui left labeled input">
              <div class="ui label">RM </div>
              <input type="text" placeholder="Amount" v-model="amount">
              <div class="ui right basic label">.00</div>
            </div>
          </div>
        </div>
        <button class="ui button primary" v-on:click="reloadCard()">Reload</button>
      </div>
    </div>
  </div>
</template>

<script>
import api from '../api'

export default {
  data () {
    return {
      notice: true,
      amount: 0.0,
      error: null,
      user: {}
    }
  },

  events: {
    'uid': function (msg) {
      this.uid = msg
      this.notice = false
      api.getUser(this, this.uid)
    }
  },

  methods: {
    reloadCard () {
      console.log('Reloading user balance...')
      this.user.balances += Number.parseFloat(this.amount + '.0')
      api.updateUser(this, this.user)
      api.go('/home')
    }
  }

}
</script>
