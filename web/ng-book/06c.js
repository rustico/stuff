angular.module("myApp", []) 

.controller('FirstController', ['$scope', 
  function($scope) {
      $scope.people = [];

      $scope.add = function(person) { 
        $scope.people.push($scope.person); 
        $scope.person = {}; 
      }; 

  }
])


;

/*

$scope.age21 = function(person) {
  return person.age > 21;
}

.filter('masNico', function() { 
  return function(input) { 
    if (input) return input + 'Nico';
  };
})


*/
