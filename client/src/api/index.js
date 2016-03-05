/**
 * Mocking client-server processing
 */
const API_LOC = 'http://localhost:3000/api'
const STUDENTS_URL = API_LOC + '/students'

export default {

  user: {
    authenticated: false
  },

  // Vue.http.get('/someUrl', [data], [options]).then(successCallback, errorCallback);
  // Vue.http.post('/someUrl', [data], [options]).then(successCallback, errorCallback);

  getUser (context, uid) {
    var options = [{
      method: 'GET',
      headers: [{
        'Access-Control-Request-Method': 'GET',
        'Access-Control-Request-Headers': 'X-Requested-With',
        'Content-Type': 'application/json'
      }]
    }]
    context.$http.get(STUDENTS_URL, uid, options).then((resp) => {
      console.log('API: \'' + JSON.stringify(resp.data) + '\'')
      return resp.data
    }).catch((err) => {
      context.error = err
    })
  },

  setUser (context, user) {
    console.log('User: ' + JSON.stringify(user))
    var options = [{
      method: 'POST',
      headers: [{
        'Access-Control-Request-Method': 'POST',
        'Access-Control-Request-Headers': 'X-Requested-With',
        'Content-Type': 'application/json'
      }]
    }]
    context.$http.post(STUDENTS_URL, user, options).then((resp) => {
      console.log('Response: ' + resp.data)
      return resp.data
    }).catch((err) => {
      context.error = err
    })
  }

}
