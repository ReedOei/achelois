# About

`achelois` is a collection of tools to make writing shell script style programs in Prolog easier.

# Quickstart

Run

```prolog
pack_install(achelois).
```

# Examples:

Run a process and store it's output in a variable, using a path other than the current directory. The last arguments are all options and can be left empty/not passed:

```prolog
?- process(path(ls), [], [path('Prolog/achelois'), output(Output)]).
Output = 'pack.pl\nprolog\nREADME.md\n' .
```

Unify `P` with the various processes of a user (similar rules exist to get other information, such as PID, command, or start time):
```prolog
?- user(roei, P).
P = process(roei, 1243, 14010, 1243, 1243, 0, '15:52', 'pts/26', '00:00:00', [bash]) ;
P = process(roei, 1455, 3299, 4205, 4205, 2, '09:57', ?, '00:08:29', ['/usr/share/spotify/spotify']) ;
P = process(roei, 1952, 14010, 1952, 1952, 0, '10:00', 'pts/7', '00:00:00', [bash]) ;
P = process(roei, 2147, 2110, 2056, 1952, 0, '10:00', 'pts/7', '00:01:17', ['/home/roei/idea-IU-181.4445.78/bin/fsnotifier64']) ;
...
```

Unify `File` with all files/directories inside another directory:

```prolog
?- walk('/home/roei/Prolog/achelois/', File).
File = '/home/roei/Prolog/achelois/pack.pl' ;
File = '/home/roei/Prolog/achelois/prolog' ;
File = '/home/roei/Prolog/achelois/prolog/achelois.pl' ;
File = '/home/roei/Prolog/achelois/prolog/build_systems.pl' ;
...
File = '/home/roei/Prolog/achelois/prolog/utility.pl' ;
false.
```

Run a Java program (that builds using Maven).
You __don't__ have to provide a fully qualified name, just the last part (suffix) of it.
This means you can just write a class name and it will find a fully qualified name (or names) which match it.
```prolog
?- java('Main', ['argument']).
```

Clone a git repo (Path and Commit terms are unified with the path it gets downloaded to and the commit).
You can also specify the path/commit if you wish.
```prolog
?- git_clone('https://github.com/ReedOei/achelois', Path, Commit).
Clonage dans 'achelois'...
remote: Counting objects: 177, done.
remote: Compressing objects: 100% (16/16), done.
remote: Total 177 (delta 13), reused 24 (delta 13), pack-reused 148
Réception d'objets: 100% (177/177), 35.72 KiB | 0 bytes/s, fait.
Résolution des deltas: 100% (100/100), fait.
Vérification de la connectivité... fait.
Path = achelois,
Commit = '1570038399540fc6979db40c306c6d750f2e2737' .
```

Copy files to a remote computer (assumes you have the SSH keys and everything set up).
Can optionally specify the destination (will be unified with the default directory you SSH into otherwise).
```prolog
?- scp('path', 'user@host', Path).
Path = '/home/user'.
```

Compile a Java project (using Maven, Gradle, Ant, or Make).
The plan is to expand this so that it will work with other languages.
```prolog
?- compiles('/home/roei/Java/eunomia').
```

