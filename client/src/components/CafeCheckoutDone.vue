<template>
<div class="pusher">
  <div class="ui grid middle align container">
    <div class="row">
      <div class="three wide column"></div>
      <div class="ten wide column">

        <div class="ui icon blue message">
          <i class="info icon"></i>
          <div class="content">
            <div class="header">
              Success
            </div>
            <p>You checkout has been processed</p>
            <button class="ui green button" v-link="{ path: '/cafe' }">Go To Cafe?</button>
          </div>
        </div>

      </div>
      <div class="three wide column"></div>
    </div>
  </div>
</div>
</template>
