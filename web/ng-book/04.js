angular.module("myApp", []) 

.run(function($rootScope) {
    $rootScope.name = "Tin";
})

.controller('MyController', ['$scope', 
  function($scope) {
    $scope.name = "Cho";
  }
])

;


/*

.controller('MyControllerChildren', ['$scope', 
  function($scope) {
    $scope.name = "TinCho";
  }
])

- They provide observers to watch for model changes
- They provide the ability to propagate model changes through the applicationaswellasoutside
the system to other components
- They can be nested such that they can isolate functionality and model properties
- They provide an execution environment in which expressions are evaluated

*/
