:- module(build_systems, [build_system/1, goal/1, goal_args/3, exe_name/2, builds_with/2, success_string/2,
                          maven_module/2, maven_modules/2, classpath/2, classpath/3, goal_files/3,
                          quick_classpath/2, quick_classpath/3]).

:- use_module(library(filesex)).

:- use_module(utility).

build_system(maven).
build_system(ant).
build_system(gradle).
build_system(make).

goal(compile).
goal(testCompile).
goal(test).
goal(package).
goal(dependencies).
goal(install).

goal_args(maven, compile, ['compile']).
goal_args(maven, testCompile, ['test-compile']).
goal_args(maven, test, ['test']).
goal_args(maven, package, ['package', '-DskipTests', '-Drat.skip']).
goal_args(maven, dependencies, ['dependency:copy-dependencies']).
goal_args(maven, install, ['install', '-fn', '-DskipTests', '-Drat.skip']).

goal_args(ant, compile, ['build']).
goal_args(ant, testCompile, ['build']).
goal_args(ant, test, ['test']).

goal_args(gradle, compile, ['assemble']).
goal_args(gradle, testCompile, ['assemble']).
goal_args(gradle, test, ['test']).

goal_args(make, compile, ['']).

% If the specified paths exist, then the goal will not be executed.
% If wish to disable this behavior, either do not write a rule for the configuration you wish to disable,
% or explicity set the configuration in question to be false.
goal_files(maven, compile, ['target/classes']).
goal_files(maven, testCompile, ['target/test-classes']).
goal_files(maven, dependencies, ['target/dependency']).

exe_name(maven, path(mvn)).
exe_name(ant, path(ant)).
exe_name(gradle, path(gradle)).
exe_name(make, path(make)).

success_string(maven, 'BUILD SUCCESS').
success_string(ant, 'BUILD SUCCESSFUL').
success_string(gradle, 'BUILD SUCCESSFUL').
success_string(make, ''). % Make doesn't have a uniform message, so we can't really tell

buildfile(maven, Path, P) :- directory_file_path(Path, 'pom.xml', P).
buildfile(ant, Path, P) :- directory_file_path(Path, 'build.xml', P).
buildfile(gradle, Path, P) :- directory_file_path(Path, 'build.gradle', P).
buildfile(make, Path, P) :- directory_file_path(Path, 'Makefile', P).

builds_with(System, Path) :- buildfile(System, Path, P), exists_file(P).

maven_module(Path, ModulePath) :-
    file_base_name(Path, 'pom.xml'),
    file_directory_name(Path, ModulePath).
maven_module(Path, ModulePath) :-
    exists_directory(Path),
    list_files(Path, Files),
    member(File, Files),
    maven_module(File, ModulePath).

maven_modules(Path, Modules) :- findall(ModulePath, maven_module(Path, ModulePath), Modules).

classpath(Path, Classpath) :-
    builds_with(System, Path),
    classpath(System, Path, Classpath).
classpath(maven, Path, Classpath) :-
    tmp_file('cp', OutputPath),
    atom_concat('-Dmdep.outputFile=', OutputPath, OutputArg),
    process(path(mvn), ['dependency:build-classpath', OutputArg], [path(Path)]),
    read_file(OutputPath, [Classpath|_]),
    delete_file(OutputPath).

find_jars(Path, Jars) :-
    findall(Jar,
        (
            walk(Path, Jar),
            file_name_extension(_, 'jar', Jar)),
        Jars).

quick_classpath(Path, Classpath) :-
    builds_with(System, Path),
    quick_classpath(System, Path, Classpath).
quick_classpath(maven, Path, Classpath) :-
    directory_file_path(Path, 'target/classes', Classes),
    directory_file_path(Path, 'target/test-classes', TestClasses),
    directory_file_path(Path, 'target', TargetPath),

    find_jars(TargetPath, Jars),
    append([Classes, TestClasses], Jars, AllPaths),

    atomic_list_concat(AllPaths, ':', Classpath).

