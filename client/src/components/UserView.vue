
<template>
  <div class="ui raised segment">
    <a class="ui blue ribbon label">Profile</a>
    <br/><br/>
<!--
    <div>
      <pre>
        {{ $data | json }}
      </pre>
    </div>
 -->
    <div class="ui icon error message" v-show="no_user">
      <i class="warning circle icon"></i>
      <div class="content">
        <div class="header">
          No such user
        </div>
        <p>Please add the new user below</p>
      </div>
    </div>

    <form id="student-form" class="ui form">
      <h4 class="ui dividing header">Student Information</h4>
      <div class="field">
        <label>Name</label>
        <div class="two fields">
          <div class="required field ">
            <input id="first" name="first" placeholder="First Name" type="text" v-model="user.first" />
            <!-- <div class="ui basic red pointing prompt label transition visible">Please enter your first name</div> -->
          </div>
          <div class="required field ">
            <input id="last" name="last" placeholder="Last Name" type="text" v-model="user.last" />
            <!-- <div class="ui basic red pointing prompt label transition visible">Please enter your last name</div> -->
          </div>
        </div>
      </div>
      <div class="field">
        <label>Address</label>
        <div class=" field">
          <input id="address1" name="address1" placeholder="Address Line 1" type="text" v-model="user.address1" />
          <span>&nbsp;</span>
        </div>
        <div class=" field">
          <input id="address2" name="address2" placeholder="Address Line 2" type="text" v-model="user.address2" />
          <span>&nbsp;</span>
        </div>
        <div class="fields">
          <div class=" ten wide field">
            <input id="city" name="city" placeholder="City" type="text" v-model="user.city" />
            <!-- <div class="ui basic red pointing prompt label transition visible"></div> -->
          </div>
          <div class=" six wide field">
            <input id="postcode" name="postcode" placeholder="Postcode" v-model="user.postcode" />
            <!-- <div class="ui basic red pointing prompt label transition visible"></div> -->
          </div>
        </div>
      </div>

      <h4 class="ui dividing header">Personal Information</h4>
      <div class="field">
        <label>ID Number</label>
        <div class="two fields">
          <div class="field ">
            <input id="id_num" name="id_num" placeholder="ID Number" type="text" v-model="user.id_num" />
            <!-- <div class="ui basic red pointing prompt label transition visible">Please enter your National ID</div> -->
          </div>
          <div class="field ">
            <input id="student_num" name="student_num" placeholder="Student ID #" type="text" v-model="user.student_num" />
            <!-- <div class="ui basic red pointing prompt label transition visible">Please enter your Student ID</div> -->
          </div>
        </div>
      </div>
      <div id="add-student" class="ui green submit button" v-show="can_add" v-on:click="addUser">Add Student</div>
    </form>
  </div>
</template>

<script>
import api from '../api'

export default {

  ready () {
  },

  props: [],

  data () {
    return {
      uid: null,
      no_user: null,
      can_add: null,
      result: null,

      user: {
        first: '',
        last: '',
        address1: '',
        address2: '',
        city: '',
        postcode: '',
        id_num: '',
        student_num: '',
        card_uid: ''
      }
    }
  },

  events: {
    'uid': function (msg) {
      this.uid = msg
      api.getUser(this, this.uid)
    }
  },

  methods: {
    addUser () {
      // this.can_add = false
      // this.no_user = false
      console.log('UserView: user = ' + JSON.stringify(this.user))
      api.addUser(this, this.uid, this.user)
    }
  }

}
</script>