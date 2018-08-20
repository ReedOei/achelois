:- module(maven_xml, []).

:- use_module(library(sgml)).
:- use_module(xml).

load_pom(File, Pom) :- load_xml(File, Pom, []).

