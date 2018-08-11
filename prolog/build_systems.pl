:- module(build_systems, [build_system/1, goal/1, goal_name/3, exe_name/2, builds_with/2, success_string/2,
                          maven_module/2, maven_modules/2, extra_args/3, classpath/2, classpath/3]).

:- use_module(library(filesex)).

:- use_module(utility).

% TODO: keep track of failing builds, make the compile output something other than compile-output (mkae it goal dependent)
%       then we can use it to cache the results of doing a build
% Get classpath for build system/project

build_system(maven).
build_system(ant).
build_system(gradle).

goal(compile).
goal(testCompile).
goal(test).
goal(package).
goal(dependencies).
goal(install).

goal_name(maven, compile, 'compile').
goal_name(maven, testCompile, 'test-compile').
goal_name(maven, test, 'test').
goal_name(maven, package, 'package').
goal_name(maven, dependencies, 'dependency:copy-dependencies').
goal_name(maven, install, 'install').

goal_name(ant, compile, 'build').
goal_name(ant, testCompile, 'build').
goal_name(ant, test, 'test').

goal_name(gradle, compile, 'assemble').
goal_name(gradle, testCompile, 'assemble').
goal_name(gradle, test, 'test').

classpath(Path, Classpath) :-
    builds_with(System, Path),
    classpath(System, Path, Classpath).
classpath(maven, Path, Classpath) :-
    tmp_file('cp', OutputPath),
    atom_concat('-Dmdep.outputFile=', OutputPath, OutputArg),
    read_process(Path, path(mvn), ['dependency:build-classpath', OutputArg], _),
    read_file(OutputPath, [Classpath|_]),
    delete_file(OutputPath).

quick_classpath(Path, Classpath) :-
    builds_with(System, Path),
    quick_classpath(System, Path, Classpath).
quick_classpath(maven, Path, Classpath) :-
    directory_file_path(Path, 'target/classes', Classes),
    directory_file_path(Path, 'target/test-classes', TestClasses),
    directory_file_path(Path, 'target/dependency', Dependencies),
    findall(Jar, (walk(Dependencies, Jar), file_name_extension(_, 'jar', Jar)), Jars),
    append([Classes, TestClasses], Jars, AllPaths),
    atomic_list_concat(AllPaths, ':', Classpath).

% If the specified paths exist, then the goal will not be executed.
% If wish to disable this behavior, either do not write a rule for the configuration you wish to disable,
% or explicity set the configuration in question to be false.
goal_files(maven, compile, ['target/classes']).
goal_files(maven, testCompile, ['target/test-classes']).
goal_files(maven, dependencies, ['target/dependency']).

% If some build system needs additional arguments for any goal,
% this is used to include them.
extra_args(_, _, []).
extra_args(maven, dependencies, ['install', '-fn', '-DskipTests', '-Drat.skip']).
extra_args(maven, package, ['-DskipTests']).

exe_name(maven, path(mvn)).
exe_name(ant, path(ant)).
exe_name(gradle, path(gradle)).

success_string(maven, 'BUILD SUCCESS').
success_string(ant, 'BUILD SUCCESSFUL').
success_string(gradle, 'BUILD SUCCESSFUL').

buildfile(maven, Path, P) :- directory_file_path(Path, 'pom.xml', P).
buildfile(ant, Path, P) :- directory_file_path(Path, 'build.xml', P).
buildfile(gradle, Path, P) :- directory_file_path(Path, 'build.gradle', P).

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

