:- module(maven_xml, [pom/2, add_artifact/4, artifacts/3]).

:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(xml).

pom(File, Pom) :-
    var(Pom) -> load_xml(File, Pom, []);

    setup_call_cleanup(
        open(File, write, Stream),
        xml_write(Stream, Pom, []),
        close(Stream)).

config_option(element(Name, [], [V]), Name=V).

artifact_type(Artifact, ArtifactType) :-
    Artifact =.. [ArtifactType, _,  _, _, _].

coords(Artifact, Coords) :-
    Artifact =.. [_, Coords, _, _, _].

configuration(Artifact, Configuration) :-
    Artifact =.. [_, _, Configuration, _, _].

dependencies(Artifact, Dependencies) :-
    Artifact =.. [_, _, _, Dependencies, _].

artifact_element(Artifact, Element) :-
    (
        nonvar(Artifact) -> Artifact =.. [ArtifactType, GroupId:ArtifactId:Version, Configuration, Dependencies, Element];

        true
    ),

    Element = element(ArtifactType, [], Tags),

    member(element(groupId, [], [GroupId]), Tags),
    member(element(artifactId, [], [ArtifactId]), Tags),

    % The following are optionally part of the artifact

    % Version
    (
        not(Version = ''), member(element(version, [], [Version]), Tags) -> true;

        Version = ''
    ),

    % Configuration
    (
        not(Configuration = []), member(element(configuration, [], ConfigurationOptions), Tags) ->
            maplist(config_option, ConfigurationOptions, Configuration);

        Configuration = []
    ),

    % Dependencies
    (
        not(Dependencies = []), member(element(dependencies, [], DependencyElements), Tags) ->
            maplist(artifact_element, Dependencies, DependencyElements);

        Dependencies = []
    ),

    (
        var(Artifact) -> Artifact =.. [ArtifactType, GroupId:ArtifactId:Version, Configuration, Dependencies, Element];

        true
    ).

artifacts(Pom, Container, Artifact) :-
    xml_element(element(Container, _, Artifacts), Pom),
    member(ArtifactElement, Artifacts),
    artifact_element(Artifact, ArtifactElement).

artifact_adder(Container, Artifact, element(Container, Attrs, Children), element(Container, Attrs, NewChildren)) :-
    artifact_element(Artifact, ArtifactElement),
    select(ArtifactElement, NewChildren, Children).

add_artifact(Container, Artifact, Pom, NewPom) :-
    % If the plugins element exists
    xml_element(element(plugins, _, _), Pom) ->
        modify_element(maven_xml:artifact_adder(Container, Artifact), Pom, NewPom);
    % otherwise, we need to add it:

    [element(E, Attrs, Children)] = Pom,
    select(element(build, [], [element(plugins, [], [])]), NewChildren, Children),
    add_artifact(Container, Artifact, [element(E, Attrs, NewChildren)], NewPom).

