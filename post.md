# Writing a Forth

After my Lisp, I'm now working on a stack-based language like Forth.
I enjoy working on 'extreme' languages, because applying a principle everywhere teaches you where the principle makes sense and what the limitations are.
[Extremist Programming](http://blog.ezyang.com/2012/11/extremist-programming/)

In Forth and other concatenative languages, this principle is the stack.
[explanation of stack-based languages]

Forth aims for 'minimal overall complexity', often at the cost of
convenience, compatibility and safety. [Thoughtful Programming](http://www.ultratechnology.com/forththoughts.htm)


Like Lisp, Forth has very little syntax, but powerful metaprogramming capabilities. Comment syntax and basic control flow can actually be defined from inside the language. [My history with Forth & stack machines](http://yosefk.com/blog/my-history-with-forth-stack-machines.html)

## Demo


## Implementation
- No arguments, pure side effects
- Compile/interpret mode
- Dictionary

## Conclusion
Additions and ideas:
- Lambda's (Factor)
- Return stack
- Control flow
