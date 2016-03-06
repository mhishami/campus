<template>
  <div class="ui grid middle align stackable container">
    <div class="ui ordered steps">
      <div class="completed step">
        <div class="content">
          <div class="title">Selections</div>
          <div class="description">Choose your items</div>
        </div>
      </div>
      <div class="active step">
        <div class="content">
          <div class="title">Pay</div>
          <div class="description">Scan card for payment</div>
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
            Touch your card for payment
          </div>
          <p>We're fetching the balance info for you</p>
        </div>
      </div>

      <div class="ui icon negative message" v-show="insuf_funds">
        <i class="warning icon"></i>
        <div class="content">
          <div class="header">
            We're sorry, we can't accept your card
          </div>
          <br/>
          <p>
            Your card balance is <strong>{{ user.balances | currency 'RM '}}</strong><br/>
            You have insufficient balance. Please reload your card
          </p>
          <div class="extra content">
            <button class="ui green button" v-on:click="reloadCard()">
              <i class="arrow right icon"></i> Reload Your Card?
            </button>
          </div>
        </div>
      </div>

    </div>
    <div class="eight wide column">
    <div class="ui segment">
      <table class="ui single line table">
        <thead>
          <tr>
            <th>Item</th>
            <th>Name</th>
            <th>Unit Price</th>
            <th>Quantity</th>
            <th>Sub Total</th>
          </tr>
        </thead>
        <tbody v-for="(index, item) in cart">
          <tr>
            <td>{{ index + 1 }}</td>
            <td>{{ item.name }}</td>
            <td>{{ item.price | currency 'RM '}}</td>
            <td>{{ item.quantity }}</td>
            <td>{{ item.price * item.quantity | currency 'RM '}}</td>
          </tr>
        </tbody>
        <tfoot>
          <tr>
            <td>GST</td>
            <td></td>
            <td></td>
            <td></td>
            <td>{{ gst | currency 'RM ' }}</td>
          </tr>
          <tr>
            <td><h4>TOTAL</h4></td>
            <td></td>
            <td></td>
            <td></td>
            <td><h4>{{ total | currency 'RM ' }}</h4></td>
          </tr>
        </tfoot>
      </table>
    </div>
    </div>
  </div>

</template>

<script>
import api from '../api'
import router from '../main'

export default {
  data () {
    return {
      uid: null,
      notice: true,
      insuf_funds: false,
      user: null
    }
  },

  computed: {
    total: function () {
      return window.localStorage.getItem('total')
    },

    cart: function () {
      return JSON.parse(window.localStorage.getItem('cart'))
    },

    gst: function () {
      return window.localStorage.getItem('gst')
    }
  },

  events: {
    'uid': function (msg) {
      this.uid = msg
      this.notice = false
      api.getUser(this, this.uid)

      // check for the balance
      if (this.user.balances <= this.total) {
        this.insuf_funds = true
      }
    }
  },

  methods: {
    reloadCard () {
      router.go('/reload')
    }
  }

}
</script>