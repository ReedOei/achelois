:- module(git, [git_clone/3, clone_project/3, git_commits/2, git_commits/3,
                git_branches/2, git_branch/2, git_remote/3, git_pull/1, git_pull/3]).

:- use_module(utility).

clone_project(Url, Commit, Path) :-
    file_base_name(Url, TempPath),
    string_concat_list([TempPath, "-", Commit], Path),

    process(path(git), ["clone", Url, Path]),
    process(path(git), ["checkout", Commit], [path(Path)]).

git_clone(Url, Path, Commit) :-
    var(Commit),
    file_base_name(Url, Path),

    process(path(git), ['clone', Url, Path]),
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
    process(path(git), ['--no-pager', 'log', '--format=%H'], [path(Path), lines(AllCommits)]),
    include(\=(''), AllCommits, Commits).

git_commits(Path, Subpath, Commits) :-
    process(path(git), ['--no-pager', 'log', '--format=%H', '--', Subpath], [path(Path), lines(AllCommits)]),
    include(\=(''), AllCommits, Commits).

% Gets the remote for some branch
git_remote(RepoPath, Branch, Remote) :-
    var(Branch),
    git_branch(RepoPath, Branch),
    git_remote(RepoPath, Branch, Remote).
git_remote(RepoPath, Branch, Remote) :-
    nonvar(Branch),
    atomic_list_concat(['branch', Branch, 'remote'], '.', Option),
    process(path(git), ['config', '--local', Option], [path(RepoPath), lines([Remote|_])]).

% Lists all branches from all remotes
git_branches(RepoPath, Branches) :-
    process(path(git), ['branch', '-r', '--no-color', '--no-abbrev'], [path(RepoPath), lines(Lines)]),

    findall(branch(Remote, BranchName),
    (
        member(Line, Lines),
        trim(Line, Trimmed),
        atomic_list_concat([Remote, BranchName], '/', Trimmed)
    ), Branches).

% Shows the current branch
git_branch(RepoPath, Branch) :-
    process(path(git), ['branch', '--no-color', '--no-abbrev'], [path(RepoPath), lines(Lines)]),
    member(Line, Lines),
    atom_concat('* ', Branch, Line).

% Perform a pull (default remote onto default branch)
git_pull(RepoPath) :- git_pull(RepoPath, _, _).
git_pull(RepoPath, Remote, Branch) :-
    var(Remote),
    git_remote(RepoPath, Branch, Remote),
    git_pull(RepoPath, Branch, Remote).
git_pull(RepoPath, Remote, Branch) :-
    nonvar(Remote),
    var(Branch),
    git_branch(RepoPath, Branch),
    git_pull(RepoPath, Remote, Branch).
git_pull(RepoPath, Remote, Branch) :-
    nonvar(Remote),
    nonvar(Branch),
    process(path(git), ['pull', Remote, Branch], [path(RepoPath), output(_)]).

