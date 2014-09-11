angular.module("myApp", []) 

.controller('FirstController', ['$scope', 
  function($scope) {
      $scope.people = [];

      $scope.add = function(person) { 
        $scope.people.push($scope.person); 
        $scope.person = {}; 
      }; 

      $scope.remove = function(person) { 
        var index = $scope.people.indexOf(person);
        console.log(index);
        $scope.people.splice(index, 1);
      }; 

  }
])
