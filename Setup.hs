import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Verbosity

main = defaultMainWithHooks $ simpleUserHooks { postBench = runhp2ps}

--runhp2ps :: Args -> BenchmarkFlags -> PackageDescription -> LocalBuildInfo -> IO ()
runhp2ps (name:_) _ _ _ = do
    runProgramInvocation silent hp2ps
    runProgramInvocation silent rmaux
    runProgramInvocation silent rmhp
    where hp2ps = simpleProgramInvocation "hp2ps" ["-e8in","-c",name ++ ".hp"]
          rmaux = simpleProgramInvocation "rm" [name ++ ".aux"]
          rmhp = simpleProgramInvocation "rm" [name ++ ".hp"]
runhp2ps [] _ _ _ = return ()
