<template>
  <div class="ui inverted vertical masthead center aligned segment">
    <div class="ui text container">
      <h1 class="ui inverted header">
        Imagine If You Could Identify A Person
      </h1>
      <h2>What other apps you can do with it...</h2>
      <div class="ui huge primary button" v-link="{ path: '/checkin' }">Get Started <i class="right arrow icon"></i></div>
    </div>
  </div>

  <div class="ui vertical stripe segment">
    <div class="ui middle aligned stackable grid container">
      <div class="row">
        <div class="eight wide column">
          <h3 class="ui header">We Authenticate</h3>
          <p>Integrating with other Enterprise Systems are our core business.</p>
          <p>When you can integrate Identity to other systems, the possibilities are limitless</p>
        </div>
        <div class="eight wide column">
          <img src="../assets/crm.jpg">
        </div>
      </div>

      <div class="row">
        <div class="eight wide column">
          <img src="../assets/integration.jpg">
        </div>
        <div class="eight wide column">
          <h3>We Integrate</h3>
          <p>Integration with other IT Systems can be a daunting task for anyone to begin with as it involves downtime. </p>
          <p>However, through our years of expertise, we can mitigate the issues by working with your current vendors and work together with them to achieve the results required.</p>
        </div>
      </div>
    </div>
  </div>

</template>

<script>
export default {
  data () {
  },

  methods: {
  }

}
</script>

<style type="text/css">

.masthead .text {
  margin-top: 200px;
  padding-bottom: 100px;
  color: #444;
}

.masthead .text .header {
  color: #222;
}

.vertical .middle .row {
  margin-top: 50px;
  margin-bottom: 50px;
}

.masthead {
  background-image: url("../assets/greenfield.jpg");
  background-repeat: no-repeat;
  background-position: center;
  background-size: cover;
  width: 100%;
}

</style>
