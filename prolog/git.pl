:- module(git, [git_clone/3, clone_project/3, git_commits/2, git_commits/3]).

% TODO: Add more git commands:
%       - checkout (split from clone command)
%       - log

clone_project(Url, Commit, Path) :-
    file_base_name(Url, TempPath),
    string_concat_list([TempPath, "-", Commit], Path),

    run_process(path(git), ["clone", Url, Path]),
    read_process(Path, path(git), ["checkout", Commit], _).

git_clone(Url, Path, Commit) :-
    var(Commit),
    file_base_name(Url, Path),

    run_process(path(git), ['clone', Url, Path]),
    read_process(Path, path(git), ['log', '--format=%H', '-n1'], Output),
    atomic_list_concat([Commit|_], '\n', Output).

git_clone(Url, Path, Commit) :-
    nonvar(Commit),
    file_base_name(Url, TempPath),
    atomic_list_concat([TempPath, '-', Commit], Path),

    read_process(path(git), ['clone', Url, Path], _),
    read_process(Path, path(git), ['checkout', Commit], _).

git_commits(Path, Commits) :-
    read_process(Path, path(git), ['--no-pager', 'log', '--format=%H'], Output),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

git_commits(Path, Subpath, Commits) :-
    read_process(Path, path(git), ['--no-pager', 'log', '--format=%H', '--', Subpath], Output),
    atomic_list_concat(AllCommits, '\n', Output),
    include(\=(''), AllCommits, Commits).

