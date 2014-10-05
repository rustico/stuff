angular.module('myApp', [])

.directive('myDirective', function() {
  return {
    restrict: 'AE',
    replace: true,
    scope: {
      myUrl: '@',
      myLinkText: '@'
    },
    template: '<a href="{{myUrl}}">{{myLinkText}}</a>'
  };
});
