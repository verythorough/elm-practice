// require(d) gulp for compatibility with sublime-gulp.
var gulp = require('gulp');
var browserSync = require('browser-sync');
var reload = browserSync.reload;
var path = require('path');
var swPrecache = require('sw-precache');

gulp.task('generate-service-worker', function(callback) {
  var rootDir = './';

  swPrecache.write(path.join(rootDir, 'sw.js'), {
    staticFileGlobs: [
      rootDir + '/index.html',
      rootDir + '/style.css',
      rootDir + '/elm.js',
      rootDir + '/service-worker-registration.js'
    ],
    stripPrefix: rootDir
  }, callback);
});

// Watch files for changes & reload
gulp.task('serve', ['generate-service-worker'], function() {
  browserSync({
    port: 5000,
    notify: false,
    logPrefix: 'PSK',
    snippetOptions: {
      rule: {
        match: '<span id="browser-sync-binding"></span>',
        fn: function(snippet) {
          return snippet;
        }
      }
    },
    // Run as an https by uncommenting 'https: true'
    // Note: this uses an unsigned certificate which on first access
    //       will present a certificate warning in the browser.
    // https: true,
    server: {
      baseDir: ['./']
    }
  });

  gulp.watch(['public/**/*.{js,html,css,png,jpg,gif}'], ['generate-service-worker', reload]);
});
