# cfg
Private config for emacs

I have tried almost everything while searched for solution of
"emacs c/c++ ide".
When we talk about IDE, we talk about 
1. file/buffer management
2. nice debugging support.
3. Intelegent completions and tag jumps.

Ok, point 1 is work out-of-box and can be extended by insane amount of emacs extensions.
2. I ve tuned gdb-mi/gud modules for emacs, and this config is named gdb-ok.
3. Most hard things for me.
First for all i have tried semantic (from CEDET).
This is great solution, but requires heavy setup(wasn't a problem)
And finally i have found some annoying issues like semantic could deny of parsing code if it facing
up to unexpected lexems like
#ifdef A
void somef() {
#else
void somef2() {
#endif
also semantic is slow, and it's not always parses c++ syntax correctly.


2nd i have trying gtags.
it's really fast, faster than anything i saw, and it's great for context-independed languages.
i still using it for navigating throw c sources. Disadvantags of gtags(imho) is ugly work with 
GTAGSLIBPATH... However you could handle it correctly via elisp.

I dont really familar with cscope, so i will not talk about it.

next thing i have tryed is company-clang.
nothing special, but it's slow for large projects.

next thing i have found was rtags.
rtags is really great. It has INSANE functionality, with huge amount of very instresing features.
And rtags provides tagging and compliting functionality in one tool....
But, for large projects it is slow.

finally, i have found irony-mode. It's fast (looks like it caches completions results). and It's 
the best of complitions tools i saw for emacs. It also has some build-tools integration for today.
I wrote citags (lisp and binary) for code navigation while i'm using irony mode, but if irony will 
provide this functionality i will switch to irony.


