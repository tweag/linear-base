# Linear Streams

A streaming library based off of [streaming] that uses linear types to enforce
invariants of how streams are used. These invariants make sure that resources
are used correctly and as expected, e.g., file handles are not read from after
they are closed from "old" stream references.

See [Design.md] for the development process.

[Design.md]: https://github.com/tweag/linear-streams/blob/master/Design.md
[streaming]: https://github.com/haskell-streaming/streaming
