import {
  ADD_TO_CART,
  CHECKOUT_REQUEST,
  CHECKOUT_SUCCESS,
  CHECKOUT_FAILURE
} from '../mutation-types'

const state = {
  added = []
}

const mutations = {
  [ADD_TO_CART] (state, productId) {
    const record = state.added.find( p => p.id === productId)
    if (!record) {
      state.added.push({
        id: productId,
        quantity: 1
      })
    } else {
      record.quantity++
    }
  }
}

export default {
  state,
  mutations
}
