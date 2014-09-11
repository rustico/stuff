angular.module("myApp", []) 

.controller('FirstController', ['$scope', 
  function($scope) {
      $scope.people = [];
      $scope.submitted = false;

      $scope.addPerson = function() { 
        if ($scope.person_form.$valid) { 
          $scope.people.push($scope.person); 
          $scope.person_form.$setPristine();
          $scope.person = {}; 
          $scope.submitted = false;
        } else {
          $scope.submitted = true;
        }
      }; 

      $scope.remove = function(person) { 
        var index = $scope.people.indexOf(person);
        console.log(index);
        $scope.people.splice(index, 1);
      }; 

  }
])

.directive('ensureUnique', 
    function($http) { 
      return {
        require: 'ngModel',
        link: function(scope, ele, attrs, c) {
                scope.$watch(attrs.ngModel, 
                  function(n) { 
                    if (!n) return;

                    $http.get('07unique.json' ) //--disable-web-security
                      .success(function(data) { 
                        c.$setValidity('unique', data.isUnique);  
                      })

                      .error(function(data) { 
                        c.$setValidity('unique', false); 
                      }); 
                  });
              } 

     };
});
