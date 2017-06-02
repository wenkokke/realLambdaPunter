@realLambdaPunter
===

What I've implemented right now:

  - a small framework for running punters (see [Base.lhs][^Base]);
  - a small executable which starts up two `randy` punters (see [Main.hs][^Main]);
  - a simple AI, called *Randy*, which selects a random edge (see [Randy.lhs][^Randy]).

The framework doesn't quite work yet -- something with sockets closing unexpectedly.

[^Base]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Base.md
[^Main]: https://github.com/wenkokke/realLambdaPunter/blob/master/app/Main.hs
[^Randy]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Randy.md
