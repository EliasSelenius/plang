# plang

Plang is a programming language. it can be best decribed as a pragmatic language. That is to say, it is the opposite of a dogmatic language, which includes most languages.

The reason for plangs existence is because I found myself discontent with every other language. Wheter they run on virtual machines, enforce singular paradimes, are garbage collected, or have any flavor of dogma.
<!--(E.g "Thou shall always have less than 25 lines in any function", "Wrap EVERY field in a getter and setter, just in case", "Let's not make it possible to declare procedures without a class declaration and call it Java", "Dont use goto, it's scawy").-->

There are two categories of code: real and imaginary.
when you call a function, execute an expression, create a variable, or any other thing that effects the resulting machine code, you are doing real programming.
when you type public, private, protected, internal, const, readonly, make a getter and setter, you are doing imaginary programming.
An argument can be made for imaginary programming, that it helps with code-clenliness, however this is rather dogmatic thinking. Plang is a pragmatic language 


We program on real computers, and plang aims to make that fact clear.

This project consists of a parser and a transpiler for the language. Assuming the project does not get discontinued in the future, I may write a compiler for it.
The reason I wrote a transpiler in the first place, is because I lack sufficient competence to write a compiler.

## Motive
The C language is cool since it gives you low-level controll over memory and such, unfurtunately it sucks in every other way. So I wanted to make a language where I keep the good things and remove/change the bad things. 

Some C features that have been removed/changed/added are:

- header files / include directives. A function in C is global, always, except the ones that are static. By using forward declarations, include directives and header files, we can pretend these functions are not global, but they are. If you call a function that is not forward declared, the compiler will complain, but as long as there is an implementation the linker is all good. Further more, you as the programer can always lie to the compiler and forward declare a function that doesnt exist, in wich case there will be a linker error. In any case the linker will resolve externaly defined symbols. So whats the point with forward declarations? All they do is create a dissonance between the compiler and the linker.

- Function overloads. Do you rotate using quaternions or euler-angles? It doesnt matter anyway since with overloads a hypothetical rotate() function could use both. They're not needed but they're noice.

- Type inference. Working with languages that support type inference, I connot remember the last time I bothered to specify the type of a variable. Type inference is syntax sugar I cannot live without. Return types can also be inferred, I don't necessarily think this is good feature because when you create a function, you are actually doing two things: implementing the function, and designing the API, I think it's better to be specific about a functions return type when you design an API, but as you'll see, return type inference works well with localy defined functions and internal/private functions.

- Introspection capabilities. c# and java have the reflection apis, c have no form of introspection/reflection, I'd really like to get a string (or char*) representation of enum values for example. Or just know the fields of a struct at runtime. So, if required, the compiler should generate type information on structs, enums and functions. But, there is a much more powerfull language capability here, by serializing the syntax tree to disk, one can read it during runtime of the application, (or during compile-time), this gives you the most instrosepction you'll ever need.







# Features

## Type Inference

``` c
// Type-infer a variable with the "let" keyword.
let x = 1.0;
// x is float

// You can also type-infer return types
let add(float x, float y) {
    return x + y;
}
// add(x, y) is float
```
### Null literal type inference
Most languages dont allow for the null-literal to be type-infered,
however plang does allow for this.
``` rust
let p = null;
// what type could p be?
```
Reasoning: the zero-literal ("0") and the null-literal ("null") represents the same value, but the difference is that zero implies an integral type, while null implies a pointer type. Null does not however say anything about what type it is pointing to, only that it is a pointer. There already exists such a type in c. So naturally the type of the null literal is void*.

### Number literal type suffixes
// TODO: fill out section



## Localy defined functions

Functions can be declared inside other functions.
Local functions has the convinience of variable capturing aswell as namespacing

``` c
void main() {
    bool isLowerCaseLetter(char c) {
        return c >= 'a' and c <= 'z';
    }

    printf("Type your name:\n");
    char* input = getInput();
    if (isLowerCaseLetter(input[0])) {
        printf("I dont mean to be nitpicky, but your name should start with a capital letter.\n");
    }
}
```
// TODO: capturing variables

## Localy defined structs

## Function Pointers

## Modules

