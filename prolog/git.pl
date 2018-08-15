:- module(git, [git_clone/3, clone_project/3, git_commits/2, git_commits/3,
                git_checkout/2]).

clone_project(Url, Commit, Path) :-
    file_base_name(Url, TempPath),
    string_concat_list([TempPath, "-", Commit], Path),

    run_process(path(git), ["clone", Url, Path]),
    read_process(Path, path(git), ["checkout", Commit], _).

git_clone(Url, Path, Commit) :-
    var(Commit),
    file_base_name(Url, Path),

    run_process(path(git), ['clone', Url, Path]),
    git_checkout(Path, Commit).
git_clone(Url, Path, Commit) :-
    nonvar(Commit),
    file_base_name(Url, TempPath),
    atomic_list_concat([TempPath, '-', Commit], Path),

    read_process(path(git), ['clone', Url, Path], _),
    git_checkout(Path, Commit).

% Either shows the current commit if commit is no provided, or does git checkout COMMIT in the specified path
git_checkout(Path, Commit) :-
    var(Commit),
    read_process(Path, path(git), ['rev-parse', 'HEAD'], Temp),
    atom_concat(Commit, '\n', Temp).
git_checkout(Path, Commit) :-
    nonvar(Commit),
    read_process(Path, path(git), ['checkout', Commit], _).

git_commits(Path, Commits) :-
    read_process(Path, path(git), ['--no-pager', 'log', '--format=%H'], Output),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

git_commits(Path, Subpath, Commits) :-
    read_process(Path, path(git), ['--no-pager', 'log', '--format=%H', '--', Subpath], Output),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

