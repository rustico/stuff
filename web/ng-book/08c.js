angular.module('myApp', [])

.directive('clock', function() {
  return {
    restrict: 'E',
    replace: true,
    template: '<h5>{{ clock.now }}</h5>',
    controller: function($scope) {
      $scope.clock = {
        now : new Date()
      };

      var updateClock = function() {
        $scope.clock.now = new Date();
      };

      setInterval(function() {
          $scope.$apply(updateClock);
      }, 1000);

    }
  };
});
