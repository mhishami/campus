<template>
  <h2 class="ui header">
    <i class="cart icon"></i>
    <div class="content">Cart</div>
  </h2>
  <table class="ui very basic celled table">
    <thead>
      <tr>
        <th>Name</th>
        <th>Price</th>
        <th>Quantity</th>
        <th>Subtotal</th>
      </tr>
    </thead>
    <tbody v-for="item in cart">
      <tr>
        <td>{{ item.name }}</td>
        <td>{{ item.price | currency 'RM ' }}</td>
        <td>
          <input id="quantity" style="width: 30px;" v-model="item.quantity" readonly="" />
          <div class="ui buttons">
            <button class="ui mini button" v-on:click="decr(item)">-</button>
            <div class="or"></div>
            <button class="ui mini positive button" v-on:click="incr(item)">+</button>
          </div>
        </td>
        <td>{{ item.price * item.quantity | currency 'RM '}}</td>
      </tr>
    </tbody>
    <tfoot>
      <tr>
        <td><i>GST</i></td>
        <td></td>
        <td></td>
        <td><i>{{ gst | currency 'RM ' }}</i></td>
      </tr>
      <tr>
        <td><h4>TOTAL</h4></td>
        <td></td>
        <td></td>
        <td><h4>{{ total | currency 'RM ' }}</h4></td>
      </tr>
    </tfoot>
  </table>
  <div class="ui divider"></div>
  <div class="ui left action input">
    <button v-on:click="checkout" class="ui teal labeled icon button">
      <i class="cart icon"></i>
      Checkout
    </button>
    <input value="{{ total | currency 'RM ' }}" type="text" readonly="" />
  </div>

</template>

<script>
import api from '../api'

export default {

  data () {
    return {
      cart: []
    }
  },

  computed: {
    subtotal: function () {
      var sub = 0
      this.cart.forEach(function (item) {
        sub += item.price * item.quantity
      })
      return sub
    },

    gst: function () {
      return this.subtotal * 0.06
    },

    total: function () {
      return this.gst + this.subtotal
    }
  },

  methods: {
    incr (item) {
      item.quantity++
    },

    decr (item) {
      if (item.quantity > 0) {
        item.quantity--

        if (item.quantity === 0) {
          this.cart.$remove(item)
        }
      }
    },

    checkout () {
      // checkout
      console.log('Cart: ' + JSON.stringify(this.cart))
      window.localStorage.setItem('cart', JSON.stringify(this.cart))
      window.localStorage.setItem('total', this.total)
      window.localStorage.setItem('gst', this.gst)

      // route to checkout
      api.go('/checkout')
    }

  },

  events: {
    'cart': function (obj) {
      this.cart.push(obj)
    },

    'step2': function (obj) {
      console.log(obj)
    }
  }

}
</script>