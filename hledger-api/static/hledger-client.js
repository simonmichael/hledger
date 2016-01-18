var hledger = angular.module('hledger', [
  'ui.router',
  'ngResource'
])
function listToTree(list, id_field, parent_field) {
  children = function(list, parent_id) {
    return $.grep(list,
      function(element) {
        return element[parent_field] === parent_id
      });
  }
  $.map(list, function(element) {
    element.children = children(list, element[id_field])
  });
  root = children(list, '');
  return root;
}

hledger.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/accounts");
  $stateProvider
    .state('accounts', {
      url: "/accounts",
      templateUrl: "accounts/index.html",
      controller: 'AccountsController'
    })
    .state('help', {
      url: "/help",
      templateUrl: "help/index.html"
    });
});

hledger.factory('Journal', function($resource) {
  return($resource("/journals/:id"));
});

hledger.controller("JournalController", function($scope, Journal) {
  Journal.query(function(data) {
    $scope.journal = data;
  });
});

hledger.factory('Account', function($resource) {
  return($resource("/accounts/:id"));
});

hledger.controller("AccountsController", function($scope, Account) {
  Account.query(function(data) {
    $scope.accounts = listToTree(data, 'aname', 'aparentname')[0].children;
  });
});
