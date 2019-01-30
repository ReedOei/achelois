:- module(git, [git_clone/3, clone_project/3, git_commits/2, git_commits/3,
                git_checkout/2]).

:- use_module(utility).

clone_project(Url, Commit, Path) :-
    file_base_name(Url, TempPath),
    string_concat_list([TempPath, "-", Commit], Path),

    run_process(path(git), ["clone", Url, Path]),
    process(path(git), ["checkout", Commit], [path(Path)]).

git_clone(Url, Path, Commit) :-
    var(Commit),
    file_base_name(Url, Path),

    run_process(path(git), ['clone', Url, Path]),
    git_checkout(Path, Commit).
git_clone(Url, Path, Commit) :-
    nonvar(Commit),
    file_base_name(Url, TempPath),
    atomic_list_concat([TempPath, '-', Commit], Path),

    process(path(git), ['clone', Url, Path]),
    git_checkout(Path, Commit).

% Either shows the current commit if commit is no provided, or does git checkout COMMIT in the specified path
git_checkout(Path, Commit) :-
    var(Commit),
    process(path(git), ['rev-parse', 'HEAD'], [path(Path), output(Temp)]),
    atom_concat(Commit, '\n', Temp).
git_checkout(Path, Commit) :-
    nonvar(Commit),
    process(path(git), ['checkout', Commit], [path(Path)]).

git_commits(Path, Commits) :-
    process(path(git), ['--no-pager', 'log', '--format=%H'], [path(Path), output(Output)]),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

git_commits(Path, Subpath, Commits) :-
    process(path(git), ['--no-pager', 'log', '--format=%H', '--', Subpath], [path(Path), output(Output)]),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

