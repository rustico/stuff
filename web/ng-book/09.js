angular.module('myApp', ['ngRoute'])

.config(['$routeProvider', function($routeProvider) {
    $routeProvider
      .when('/', {
        templateUrl: '09.home.html',
        controller: 'HomeController'
      })
      .when('/page', {
        templateUrl: '09.page.html',
        controller: 'PageController'
      });
}])


.controller('HomeController', ['$scope', 
    function($scope) {
      $scope.page = 'home';
    }
])

.controller('PageController', ['$scope', 
    function($scope) {
      $scope.page = 'page';
    }
]);
