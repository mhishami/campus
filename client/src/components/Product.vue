<template>
  <div class="four wide column">
    <div class="ui card">
      <div class="content">
        <div class="header">{{ item.name }}</div>
        <div class="description">
          {{ item.price | currency 'RM ' }}
        </div>
      </div>
      <div class="extra content">
        <button v-on:click="addToCart" class="ui green button">Add <i class="arrow right icon"></i></button>
      </div>
    </div>
  </div>
</template>

<script>
export default {

  props: ['item', 'index'],

  data () {
    return {
      name: null
    }
  },

  methods: {
    addToCart () {
      var obj = {
        id: this.item.id,
        name: this.item.name,
        price: this.item.price,
        quantity: 1
      }
      this.$dispatch('selected', obj)
      // console.log('Current item: ' + JSON.stringify(obj))
    }
  }

}
</script>