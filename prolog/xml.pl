:- module(xml, [xml_element/2, modify_element/3]).

xml_element(Element, E) :-
    is_list(E) ->
        member(Child, E),
        xml_element(Element, Child);

    Element = E.
xml_element(Element, element(_Name, _Attrs, Children)) :-
    member(Child, Children),
    xml_element(Element, Child).

modify_element(Pred, Element, NewElement) :-
    is_list(Element) ->
        select(Child, Element, TempElement),
        modify_element(Pred, Child, NewChild),
        select(NewChild, NewElement, TempElement);

    call(Pred, Element, NewElement).
modify_element(Pred, element(Name, Attrs, Children), element(Name, Attrs, NewChildren)) :-
    select(Child, Children, TempChildren),

    (
        call(Pred, Child, NewElement) -> true;

        modify_element(Pred, Child, NewElement)
    ),

    select(NewElement, NewChildren, TempChildren).

