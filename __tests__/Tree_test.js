// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Tree = require("../src/Tree.js");
var Block = require("bs-platform/lib/js/block.js");

describe("Tree", (function () {
        return Jest.test("sum", (function (param) {
                      var t = Tree.make(/* :: */Block.simpleVariant("::", [
                              1,
                              /* :: */Block.simpleVariant("::", [
                                  2,
                                  /* :: */Block.simpleVariant("::", [
                                      2,
                                      /* :: */Block.simpleVariant("::", [
                                          3,
                                          /* [] */0
                                        ])
                                    ])
                                ])
                            ]));
                      return Jest.Expect[/* toBe */2](6, Jest.Expect[/* expect */0](Tree.sum(t)));
                    }));
      }));

/*  Not a pure module */
