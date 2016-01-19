var hledger = angular.module('hledger', [
  'ui.router',
  'ngResource'
])
hledger.api_root = '/api/v1';
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
    $scope.accounts = listToTree(data, 'aname', 'aparentname')[0].children;
  });
});

hledger.filter("accountNameNode", function() {
  return function(account) {
    return account.replace(/^.*:/, '');
  };
});

