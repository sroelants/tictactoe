var gulp = require('gulp');
var sass = require('gulp-sass');
var browserSync = require('browser-sync').create();
var concat = require('gulp-concat');
var useref = require('gulp-useref');
var gulpIf = require('gulp-if');
var uglify = require('gulp-uglify');
var cssnano = require('gulp-cssnano');

// Tasks

gulp.task('copy', function() {
  return gulp.src('src/index.html')
             .pipe(gulp.dest('site/'));
});

gulp.task('sass', function() {
    return gulp.src('src/sass/*.sass')
      .pipe(sass().on('error', function(err){console.log("Shit", err);}))
      .pipe(gulp.dest('site/css'))
      .pipe(browserSync.reload({
        stream: true
        }));
});

gulp.task('browserSync', function () {
    browserSync.init({
        server: {
            baseDir: 'site'
        },
        open: false
    })
});


gulp.task('build', ['sass'], function () {
  return gulp.src('site/*.html')
             .pipe(useref())
             .pipe(gulpIf('*.js', uglify()))
             .pipe(gulpIf('*.css', cssnano()))
             .pipe(gulp.dest('dist'));
             });

// browserSync must finish before other watch tasks
gulp.task('watch', ['browserSync', 'sass', 'copy'], function() {
    gulp.watch('src/index.html', ['copy']);
    gulp.watch('src/sass/*.sass', ['sass']);
    gulp.watch('site/*.html', browserSync.reload);
    gulp.watch('site/js/*.js', browserSync.reload);
});


