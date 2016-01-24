var hledger = angular.module('hledger', [
  'ui.router',
  'ngResource'
])
hledger.api_root = '/api/v1';

hledger.config(function($stateProvider, $urlRouterProvider) {
  //$urlRouterProvider.otherwise("/");
  $stateProvider
    .state('dashboard', {
      url: ""
    })
    .state('accounts', {
      abstract: true,
      url: "/accounts",
      templateUrl: "accounts/index.html"
    })
    .state('accounts.show', {
      url: "/:id",
      templateUrl: 'accounts/show.html',
      resolve: {
        Account: 'Account',
        account: function(Account, $stateParams) {
          return Account.get($stateParams);
        }
      }
    })
    .state('help', {
      url: "/help",
      templateUrl: "help/index.html"
    });
});

hledger.factory('Journal', function($resource) {
  return($resource(hledger.api_root + "/journals/:id"));
});

hledger.controller("JournalController", function($scope, Journal) {
  Journal.query(function(data) {
    $scope.journal = data;
  });
});

hledger.factory('Account', function($resource) {
  return($resource(hledger.api_root + "/accounts/:id"));
});

hledger.controller("AccountsController", function($scope, Account) {
  Account.query(function(data) {
    $scope.accounts = data;
  });
});

hledger.filter("accountNameNode", function() {
  return function(account) {
    return account.replace(/^.*:/, '');
  };
});

