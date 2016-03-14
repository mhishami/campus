
import {router} from '../main'

const API_LOC = 'http://localhost:3000/api'
const STUDENTS_URL = API_LOC + '/students'
const AUTH_URL = API_LOC + '/auth'

export default {

  user: {
    authenticated: false
  },

  go (location) {
    router.go(location)
  },

  initUser () {
    return {
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
  },

  options (opt) {
    return [{
      method: opt,
      headers: [{
        'Access-Control-Request-Method': opt,
        'Access-Control-Request-Headers': 'accept, authorization, crossorigin',
        'Access-Control-Allow-Origin': '*',
        'Content-Type': 'application/json'
      }]
    }]
  },

  authenticate (context, user) {
    context.$http.get(AUTH_URL, this.options('GET')).then((resp) => {
      context.user = resp.data
    }).catch((error) => {
      context.error = error
    })
  },

  getUser (context, uid) {
    context.$http.get(STUDENTS_URL + '/' + uid, this.options('GET')).then((resp) => {
      if (JSON.stringify(resp.data) !== '[]') {
        context.user = resp.data
        context.no_user = false
        context.can_add = false
      } else {
        context.user = this.initUser()
        context.no_user = true
        context.can_add = true
      }
    }).catch((error) => {
      console.log('API: error = ' + error)
      context.user = this.initUser()
      context.error = error
    })
  },

  addUser (context, uid, user) {
    console.log('User: ' + JSON.stringify(user))
    user.card_uid = uid
    console.log('Sending the post request: ' + JSON.stringify(user))
    context.$http.post(STUDENTS_URL, user, this.options('POST')).then((resp) => {
      console.log('Response: ' + resp.data)
      context.result = resp.data
    })
  },

  updateUser (context, user) {
    console.log('Sending the PUT request. User: ' + JSON.stringify(user))
    context.$http.put(STUDENTS_URL, user, this.options('PUT')).then((resp) => {
      console.log('Response: ' + resp.data)
      context.result = resp.data
    }).catch((error) => {
      console.log('UpdateUser: Error -> ' + error)
      context.error = error
    })
  }

}
