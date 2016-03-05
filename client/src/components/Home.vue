<template>
  <div class="ui grid">
    <div class="three column row">
      <div class="column"></div>
      <div class="column">
        <form class="ui form">
          <h1 class="ui header">ToDo List</h1>
          <div class="field">
            <input type="text" v-model="newTodo" v-on:keyup.enter="addTodo()" />
          </div>
          <ul>
            <li v-for="todo in todos">
              <span>{{ todo.text }}</span>
              <button v-on:click="removeTodo($index)">X</button>
            </li>
          </ul>
        </form>
      </div>
      <div class="column"></div>
    </div>
  </div>
</template>

<script>
export default {
  data () {
    return {
      newTodo: '',
      todos: []
    }
  },

  methods: {
    addTodo () {
      var text = this.newTodo.trim()
      if (text) {
        this.todos.push({text: text})
        this.newTodo = ''
      }
    },

    removeTodo (index) {
      this.todos.splice(index, 1)
    }
  }
}
</script>

<style type="text/css">
  margin: 10em;
</style>
