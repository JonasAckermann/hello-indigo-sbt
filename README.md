# "Hello Pluto"

An attempt to play around with the Indigo game engine, based on the hello-indigo example. There is a [write up](https://indigoengine.io/docs/quickstart/hello-indigo) if you'd like to follow along with how the this little demo was made.

## Build and Run
```bash
sbt buildGame
```

Which will generate output similar to:

```bash
> sbt buildGame
(...)
/Users/(...)/hello-indigo-sbt/target/scala-2.13/helloindigo
dirPath: /Users/(...)/hello-indigo-sbt/target/indigo-js
Copying assets...
/Users/(...)/hello-indigo-sbt/target/indigo-js/index.html
```

Then:

1. `cd /Users/(...)/hello-indigo-sbt/target/indigo-js/`
2. `http-server -c-1`
3. Navigate to [http://127.0.0.1:8080/](http://127.0.0.1:8080/) in your browser of choice.

## Controls
`Left/Right Arrow` to move.
`Shift` to pick up a passenger.

## Next Steps
1. Score and Interface.
2. Delivery.
3. Game Over.
4. Graphical updates.
5. Fix physics.
6. Cute design. 
