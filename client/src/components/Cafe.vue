<template>
  <div class="ui grid middle align stackable container">
    <div class="ui ordered steps">
      <div class="active step">
        <div class="content">
          <div class="title">Selections</div>
          <div class="description">Choose your items</div>
        </div>
      </div>
      <div class="step">
        <div class="content">
          <div class="title">Pay</div>
          <div class="description">Scan card for payment</div>
        </div>
      </div>
    </div>
  </div>
  <div class="ui grid middle align stackable container">
    <div class="eight wide column">
      <div class="ui grid">
        <product
          v-for="product in products"
          :item="product"
          :index="$index"
          track-by="id">
        </product>
      </div>
    </div>
    <div class="eight wide column">
      <cart></cart>
    </div>
  </div>

</template>

<script>
import Product from './Product.vue'
import Cart from './Cart.vue'

export default {

  data () {
    return {
      // note: changing this line won't causes changes
      // with hot-reload because the reloaded component
      // preserves its current state and we are modifying
      // its initial state.
      products: [
        {id: 1, name: 'Nasi Lemak', price: 2.5},
        {id: 2, name: 'Roti Canai', price: 1.0},
        {id: 3, name: 'Mee Goreng', price: 4.5},
        {id: 4, name: 'Nasi Ayam', price: 4.5},
        {id: 5, name: 'Teh Tarik', price: 1.5},
        {id: 6, name: 'Milo', price: 2.0},
        {id: 7, name: 'Horlick', price: 2.5},
        {id: 8, name: 'Nescafe', price: 2.0}
      ]
    }
  },

  events: {
    'selected': function (obj) {
      this.$broadcast('cart', obj)
    }

  },

  components: {
    Product,
    Cart
  }
}

</script>
