var hledger = angular.module('hledger', [
  'ui.router',
  'ngResource'
])
hledger.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/accounts");
  $stateProvider
    .state('accounts', {
      url: "/accounts",
      templateUrl: "accounts/view.html",
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
   $scope.accounts = data;
  });
});
