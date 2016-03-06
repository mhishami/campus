<template>
  <chartist
    id-chart="ct-chart"
    ratio="ct-major-second"
    type="Line"
    :data="chartData"
    :options="chartOptions" >
  </chartist>
</template>

<script>
export default {

  data () {
    return {
      chartData: {
        labels: ['Jan', 'Feb', 'March', 'April', 'May', 'June'],
        series: [
          {
            data: [22, 18, 20, 21, 18, 20]
          }
        ]
      },
      chartOptions: {
        low: 0,
        high: 30,
        chartPadding: {
          top: 50,
          bottom: 50,
          left: 50,
          right: 50
        },
        axisY: {
          onlyInteger: true
        }
      }
    }
  }

}

</script>