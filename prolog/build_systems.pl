:- module(build_systems, [build_system/1, goal/1, goal_name/3, exe_name/2, builds_with/2, success_string/2]).

:- use_module(library(filesex)).

build_system(maven).
build_system(ant).
build_system(gradle).

goal(compile).
goal(testCompile).
goal(test).

goal_name(maven, compile, "compile").
goal_name(maven, testCompile, "test-compile").
goal_name(maven, test, "test").

goal_name(ant, compile, "build").
goal_name(ant, testCompile, "build").
goal_name(ant, test, "test").

goal_name(gradle, compile, "assemble").
goal_name(gradle, testCompile, "assemble").
goal_name(gradle, test, "test").

exe_name(maven, path(mvn)).
exe_name(ant, path(ant)).
exe_name(gradle, path(gradle)).

success_string(maven, "BUILD SUCCESS").
success_string(ant, "BUILD SUCCESSFUL").
success_string(gradle, "BUILD SUCCESSFUL").

buildfile(maven, Path, P) :- directory_file_path(Path, "pom.xml", P).
buildfile(ant, Path, P) :- directory_file_path(Path, "build.xml", P).
buildfile(gradle, Path, P) :- directory_file_path(Path, "build.gradle", P).

builds_with(System, Path) :- buildfile(System, Path, P), exists_file(P).

