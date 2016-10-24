angular.module("Treezz", [])
  .directive("decisions", function() {
    return {
      restrict: "E",
      templateUrl: "app/tree.html",
    };
  })
  .directive("node", function() {
    return {
      restrict: "E",
      scope: {
        attribute: "@",
        title: "@"
      },
      template: '<div class="node">' +
                '<div class="spine"></div>' +
                '<div class="tick {{ attribute }}"' +
                      'data-toggle="tooltip"' +
                      'title="{{ title }}"></div>' +
                '<div class="branch"></div>' +
                '<div class="decisions" ng-transclude></div>' +
                "</div>",
      transclude: true,
      link: function (scope, elem, attrs) {
        attrs.attribute = "n" + attrs.attribute;
      }
    };
  })
  .directive("leaf", function() {
    return {
      restrict: "E",
      scope: {
        class: "@",
        title: "@"
      },
      template: '<div class="leaf">' +
                '<div class="spine"></div>' +
                '<div class="tick {{ class }}"' +
                      'data-toggle="tooltip"' +
                      'title="{{ title }}"></div>' +
                '</div>',
      link: function (scope, elem, attrs) {
        attrs.class = "l" + attrs.class;
      }
    };
  })
  .directive("decision", function() {
    return {
      restrict: "E",
      scope: {
        class: "@",
        title: "@"
      },
      transclude: true,
      template: '<div class="decision">' +
                '<div class="spine"></div>' +
                '<div class="tick {{ class }}"' +
                      'data-toggle="tooltip"' +
                      'title="{{ title }}"></div>' +
                  '<div ng-transclude></div>' +
                '</div>',
      link: function (scope, elem, attrs) {
        attrs.class = "d" + attrs.class;
      }
    };
  });
