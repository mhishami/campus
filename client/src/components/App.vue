
<template>
  <nav class="navbar navbar-inverse navbar-fixed-top">
    <div class="container-fluid">
      <div class="navbar-header">
        <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
          <span class="sr-only">Toggle navigation</span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
        </button>
        <a class="navbar-brand" v-link="{ path: '/home' }">Campus App</a>
      </div>
      <div class="navbar-collapse collapse">
        <ul class="nav navbar-nav">
          <li v-link-active><a v-link="{ path: '/home', activeClass: 'active', exact: true }">Home</a></li>
          <li v-link-active><a v-link="{ path: '/checkin', activeClass: 'active', exact: true }">Checkin</a></li>
          <li v-link-active><a v-link="{ path: '/cafe', activeClass: 'active', exact: true }">Cafe</a></li>
        </ul>
      </div><!--/.nav-collapse -->
    </div><!--/.container-fluid -->
  </nav>      
  <div class="container">
    <router-view></router-view>
  </div>
</template>

<style>
  body {
    min-height: 2000px;
    padding-top: 70px;
  }
  .v-link-active { color: red; }
</style>