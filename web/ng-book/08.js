angular.module("myApp", []) 

.directive('myDirective', function() {
  return {
    restrict: 'E',
    template: '<a href="http://google.com"> Click me to go to Google</a>'
  } 
})

;

/*
replace: True
 

.directive('myDirective', function() {
  return {
    restrict: 'EAC',
    template: '<a href="http://google.com"> Click me to go to Google</a>'
  } 
})


The simplest way to think about a directive is that it is simply a function that we run on a particular DOM element. The function is expected to provide extra functionality on the element.

For instance, the ng-click directive gives an element the ability to listen for the click event and run an Angular expression when it receives the event. 

*/
