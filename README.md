# plang

It's a programming language. Plang is short for "programming language", because that's a really good name.

This project consists of a parser and a transpiler for the language. Assuming the project does not get discontinued in the future, I may write a compiler for it.
The reason I wrote a transpiler in the first place, is because I lack sufficient competence to write a compiler.

And yes... "Plang" or "Programming language" is not a good name for a programming language, I was being ironic when I said it was, but I lack sufficient competence to name my projects as well, so I have given up on that.

## Motive
The C language is cool since it gives you low-level controll over memory and such, unfurtunately it sucks in every other way. So I wanted to make a language where I keep the good things and remove/change the bad things. 

Some C features that have been removed/changed/added are:

- header files / include directives. A function in C is global, always, except the ones that are static. By using forward declarations, include directives and header files, we can pretend these functions are not global, but they are. If you call a function that is not forward declared, the compiler will complain, but as long as there is an implementation the linker is all good. Further more, you as the programer can always lie to the compiler and forward declare a function that doesnt exist, in wich case there will be a linker error. In any case the linker will resolve externaly defined symbols. So whats the point with forward declarations? All they do is create a dissonance between the compiler and the linker.

- Function overloads. Do you rotate using quaternions or euler-angles? It doesnt matter anyway since with overloads a hypothetical rotate() function could use both. They're not needed but they're noice.

- Type inference. Working with languages that support type inference, I connot remember the last time I bothered to specify the type of a variable. Type inference is syntax sugar I cannot live without. Return types can also be inferred, I don't necessarily think this is good feature because when you create a function, you are actually doing two things: implementing the function, and designing the API, I think it's better to be specific about a functions return type when you design an API, but as you'll see, return type inference works well with sub-functions and internal/private functions.

- Type arguments.

- Run time type information. If required the transpiler should generate type information on structs, enums and functions.







# Features

## Type Inference

### Locals
### Return Types
### Literals

## Sub-Functions

## Function Pointers

## Modules

