angular.module("Treezz", [])
  .directive("decisions", function() {
    return {
      restrict: "E",
      templateUrl: "data/tree.html",
    };
  });
