module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs"
    ],

    psc: {
      options: {
        main: "Main",
        modules: ["Main"]
      },
      all: {
        src: ["<%=libFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=libFiles%>"],

    watch: {
      purs: {
        files: "src/*.purs",
        tasks: ["make"],
      }
    }

  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-contrib-watch");

  grunt.registerTask("make", ["psc:all", "dotPsci"]);
  grunt.registerTask("default", ["make"]);
};
