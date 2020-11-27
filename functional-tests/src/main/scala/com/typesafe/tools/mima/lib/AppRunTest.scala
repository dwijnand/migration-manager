package com.typesafe.tools.mima.lib

import scala.util.{ Failure, Success, Try }

/** Test running the App, using library v1 or v2. */
object AppRunTest {
  def testAppRun(testCase: TestCase, direction: Direction): Try[Unit] = {
    val (lhs, rhs) = direction.ordered(testCase.outV1, testCase.outV2) // (v1, v2); or (V2, v1) if forwards-compat
    for {
      () <- testCase.compileBoth
      pending = testCase.versionedFile("testAppRun.pending").exists
      expectOk = testCase.blankFile(testCase.versionedFile(direction.oracleFile))
      //() <- testCase.compileApp(rhs)    // compile app with v2
      //() <- testCase.runMain(rhs)       // sanity check 1: run app with v2
      () <- testCase.compileApp(lhs)      // recompile app with v1
      () <- testCase.runMain(lhs)         // sanity check 2: run app with v1
      () <- testCase.runMain(rhs) match { // test: run app, compiled with v1, with v2
        case Failure(t) if !pending && expectOk   => Failure(t)
        case Success(()) if !pending && !expectOk => Failure(new Exception("expected running App to fail"))
        case _                                    => Success(())
      }
    } yield ()
  }
}
