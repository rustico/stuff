var myModule = angular.module('Angello', [])
myModule.controller('MainCtrl', function($scope, angelloModel, angelloHelper) {

    $scope.currentStory;
    $scope.types = angelloModel.getTypes();
    $scope.statuses = angelloModel.getStatuses();
    $scope.stories = angelloModel.getStories();
    $scope.typesIndex = angelloHelper.buildIndex($scope.types, 'name');
    $scope.statusesIndex = angelloHelper.buildIndex($scope.statuses, 'name');

    $scope.createStory = function() {
        $scope.stories.push({
            title: 'New Story',
            description: 'Description pending.'
        });
    }

    $scope.setCurrentStory = function(story) {
        $scope.currentStory = story;

        $scope.currentStatus = $scope.statusesIndex[story.status];
        $scope.currentType = $scope.typesIndex[story.type];
    };

    $scope.setCurrentStatus = function(status) {
        if(!!$scope.currentStory) {
            $scope.currentStory.status = status.name;
        }
    };

    $scope.setCurrentType = function(type) {
        if(!!$scope.currentStory) {
            $scope.currentStory.type = type.name;
        }
    };

});

myModule.factory('angelloHelper', function() {
    var buildIndex = function(source, property) {
        var tempArray = [];
        for(var i = 0, len = source.length; i < len; ++i) {
            tempArray[source[i][property]] = source[i];
        }

        return tempArray;
    };

    return {
        buildIndex: buildIndex
    };

});

myModule.factory('angelloModel', function() {
    var getStatuses = function() {
        var tempArray = [
            { name : 'Back Log' },
            { name : 'To Do' },
            { name : 'In Progress' },
            { name : 'Code Review' },
            { name : 'QA Review' },
            { name : 'Verified' },
            { name : 'Done' }
            ];

        return tempArray;
    };

    var getTypes = function() {
        var tempArray = [
            { name : 'Feature' },
            { name : 'Enhancement' },
            { name : 'Bug' },
            { name : 'Spike' }
        ];

        return tempArray;
    };

    var getStories = function() {
        var tempArray = [
            {
                title: 'Story 00', 
                description: 'Description pending.',
                criteria: 'Criteria pending.',
                status: 'To Do',
                type: 'Feature',
                reporter: 'Lukas',
                assignee: 'Brian'
            },
            {
                title: 'Story 01', 
                description: 'Description pending.',
                criteria: 'Criteria pending.',
                status: 'To Do',
                type: 'Feature',
                reporter: 'Lukas',
                assignee: 'Brian'
            },
            {
                title: 'Story 02', 
                description: 'Description pending.',
                criteria: 'Criteria pending.',
                status: 'To Do',
                type: 'Feature',
                reporter: 'Lukas',
                assignee: 'Brian'
            }
        ];

        return tempArray;
    };

    return {
        getStatuses: getStatuses,
        getTypes: getTypes,
        getStories: getStories
    };

});





