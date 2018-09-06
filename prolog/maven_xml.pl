:- module(maven_xml, []).

:- use_module(library(sgml)).
:- use_module(xml).

load_pom(File, Pom) :- load_xml(File, Pom, []).

modify_pom.
backup_pom.
write_pom.

add_plugin.
add_dependency.

modify_plugin.
modify_dependency.

configure_plugin.
configure_dependency.

% Allow specifying section
modify_element.
add_element.

