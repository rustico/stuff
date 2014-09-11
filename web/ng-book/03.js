// 
angular.module('myApp', []); // setter

// controller.js
angular.module('myApp')  // getter
.controller('MyController', ['$scope', 
    function($scope) {
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
]);

/*
 
The scopes of the application refer to the application model. Scopes are the execution context for expressions. The $scope object is where we define the business functinality of the application, the methods in our controllers, and properties in the views.

Scopes serve as the glue between the controller and the view. 

Scopes provide the ability to watch for model changes. They give the developer the ability to propagate model changes throughout the application by using the apply mechanism available on the scope. We define and execute expressions in the context of a scope; it is also from here that we can propagate events to other controllers and parts of the application.

*/

