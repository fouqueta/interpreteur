type lettre = char

type inputsymbols = Inputsymbols of lettre list

type stacksymbols = Stacksymbols of lettre list

type states = States of lettre list

type initialstate = Initialstate of lettre

type initialstack = Initialstack of lettre

type stack = Stack of lettre list

type transitions = 
  Transitions of (lettre * lettre * lettre * lettre * stack) list

type declarations =
  Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

type automate = 
  Automate of declarations * transitions