@realLambdaPunter
===

What I've implemented right now:

  - a small framework for running punters (see [Base.lhs][^Base]);
  - a small executable which starts up two `randy` punters (see [Main.hs][^Main]);
  - a simple AI, called *Randy*, which selects a random edge (see [Randy.lhs][^Randy]);
  - a *very slow* AI, called *Tortoise*, which selects the optimal edge (see [Tortoise.lhs][^Tortoise]);
  - a greedy AI, called *Greedo*, which selects the move with the greatest immediate benefit (see [Greedo.lhs][^Greedo]).

[^Base]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Base.md
[^Main]: https://github.com/wenkokke/realLambdaPunter/blob/master/app/Main.hs
[^Randy]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Randy.md
[^Tortoise]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Tortoise.md
[^Greedo]: https://github.com/wenkokke/realLambdaPunter/blob/master/src/LambdaPunter/Greedo.md
