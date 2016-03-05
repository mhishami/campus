<template>
  <div class="ui icon {{ type }} message">
    <i class="{{ loader }} icon"></i>
    <i class="{{ x }} icon"></i>
    <div class="content">
      <div class="header">
        {{ hdr }}
      </div>
      <p>{{ msg }}</p>
    </div>
  </div>
</template>

<script>
export default {
  ready () {
  },

  props: ['type', 'hdr', 'msg', 'loader', 'x'],

  data () {
    return {
      // hdr: 'You must register before you can do that!',
      // msg: 'Visit our registration page, then try again',
      // type: 'warning'
    }
  }
}
</script>