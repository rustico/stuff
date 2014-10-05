angular.module("myApp", []) 

.controller('FirstController', ['$scope', 
  function($scope) {
      $scope.people = [];

      $scope.addPerson = function() { 
        $scope.people.push($scope.person); 
        $scope.person = {}; 

        //$scope.person_form.$setPristine();
      }; 

      $scope.remove = function(person) { 
        var index = $scope.people.indexOf(person);
        console.log(index);
        $scope.people.splice(index, 1);
      }; 

  }
])
