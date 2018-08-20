:- module(xml, [xml_element/2, modify_element/4]).

% TODO: Create Module for modification of xml files
% Make extensible, so that we can create the maven xml module

xml_element(Element, Element).
xml_element(Element, element(_Name, _Attrs, Children)) :-
    member(Child, Children),
    xml_element(Element, Child).

modify_element(Element, Pred, Element, NewElement) :-
    call(Pred, Element, NewElement).
modify_element(Element, Pred, element(Name, Attrs, Children), element(Name, Attrs, NewChildren)) :-
    select(Element, Children, TempChildren),
    call(Pred, Element, NewElement),
    select(NewElement, NewChildren, TempChildren).

