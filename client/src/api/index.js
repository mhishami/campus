
import {router} from '../main'

const API_LOC = 'http://localhost:3000/api'
const STUDENTS_URL = API_LOC + '/students'

export default {

  user: {
    authenticated: false
  },

  go (location) {
    router.go(location)
  },

  getUser (context, uid) {
    var options = [{
      method: 'GET',
      headers: [{
        'Access-Control-Request-Method': 'GET',
        'Access-Control-Request-Headers': 'X-Requested-With',
        'Content-Type': 'application/json'
      }]
    }]
    context.$http.get(STUDENTS_URL + '/' + uid, options).then((resp) => {
      context.user = resp.data

      if (JSON.stringify(resp.data) !== '[]') {
        context.no_user = false
        context.can_add = false
      } else {
        context.no_user = true
        context.can_add = true
      }
    }).catch((error) => {
      context.error = error
    })
  },

  addUser (context, uid, user) {
    // console.log('User: ' + JSON.stringify(user))
    var options = [{
      method: 'POST',
      headers: [{
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'accept, authorization, crossorigin',
        'Access-Control-Allow-Origin': '*',
        'Content-Type': 'application/json'
      }]
    }]
    console.log('Sending the post request...')
    user.card_uid = uid
    context.$http.post(STUDENTS_URL, user, options).then((resp) => {
      // console.log('Response: ' + resp.data)
      context.result = resp.data
    })
  },

  updateUser (context, user) {
    var options = [{
      method: 'PUT',
      headers: [{
        'Access-Control-Request-Method': 'PUT',
        'Access-Control-Request-Headers': 'accept, authorization, crossorigin',
        'Access-Control-Allow-Origin': '*',
        'Content-Type': 'application/json'
      }]
    }]
    console.log('Sending the PUT request. User: ' + JSON.stringify(user))
    context.$http.put(STUDENTS_URL, user, options).then((resp) => {
      console.log('Response: ' + resp.data)
      context.result = resp.data
    }).catch((error) => {
      console.log('UpdateUser: Error -> ' + error)
      context.error = error
    })
  }

}
