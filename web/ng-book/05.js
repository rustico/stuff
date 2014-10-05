angular.module("myApp", []) 


.controller('FirstController', ['$scope', 
  function($scope) {

      $scope.counter = 0;

      $scope.add = function(amount) { $scope.counter += amount; }; 

      $scope.subtract = function(amount) { $scope.counter -= amount; };
  }
]);

/* 
 
We use it to set up an initial state and to add custom behavior to the scope object.

*/
