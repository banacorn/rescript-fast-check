open Mocha
open Arbitrary
open Property

let testParams1 = Parameters.t(
  ~endOnFailure=true,
  ~examples=[[true]],
  ~interruptAfterTimeLimit=999,
  ~logger=s => Js.log(s),
  ~markInterruptAsFailure=true,
  ~maxSkipsPerRun=999,
  ~numRuns=1,
  ~path="",
  ~seed=3,
  ~skipAllAfterTimeLimit=999,
  ~timeout=999,
  ~unbiased=false,
  ~verbose=true,
  (),
)

let validateRunDetails = cr => {
  let assertNotNull = v =>
    // magic in, magic out
    switch v->Obj.magic->Js.Nullable.toOption {
    | Some(_) => ()
    | None => Js.Exn.raiseError("value was null")
    }
  switch toResult(cr) {
  | Passed => ()
  | Failed(r) =>
    r.counterexample->assertNotNull
    r.counterexamplePath->assertNotNull
    r.error->assertNotNull
    r.failures->assertNotNull
    r.numRuns->assertNotNull
    r.numShrinks->assertNotNull
    r.numSkips->assertNotNull
    r.seed->assertNotNull
    r.runConfiguration->assertNotNull
    ()
  }
}

// TODO: make each test do a simple verification that the arbitrary API calls have mapped correctly
describe("sync property checks", () => {
  let eq = i => i === i
  let eq2 = (i, j) => i === i && j === j
  let eq3 = (i, j, k) => i === i && (j === j && k === k)
  let eq4 = (i, j, k, l) => i === i && (j === j && (k === k && l === l))
  let eq5 = (i, j, k, l, m) => i === i && (j === j && (k === k && (l === l && m === m)))
  open Property.Sync
  it("sync assert_", () => {
    assert_(property1(boolean(), eq))
    assert_(property2(boolean(), boolean(), eq2))
    assert_(property3(boolean(), boolean(), boolean(), eq3))
    assert_(property4(boolean(), boolean(), boolean(), boolean(), eq4))
    assert_(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  })
  it("sync assert with params", () => {
    assertParams(property1(boolean(), eq), testParams1)
    assertParams(property2(boolean(), boolean(), eq2), Parameters.t(~examples=[(true, true)], ()))
    assertParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )
    assertParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )
    assertParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )
  })
  it("sync check pass", () => check(property1(boolean(), eq))->validateRunDetails)
  it("sync check fail", () => check(property1(boolean(), _ => false))->validateRunDetails)
  it("sync check with params", () => {
    checkParams(property1(boolean(), eq), testParams1)->validateRunDetails
    checkParams(
      property2(boolean(), boolean(), eq2),
      Parameters.t(~examples=[(true, true)], ()),
    )->validateRunDetails
    checkParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )->validateRunDetails
    checkParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )->validateRunDetails
    checkParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )->validateRunDetails

    pre(true)
  })
  it("sync FcAssert", () => {
    FcAssert.sync(property1(boolean(), eq))
    FcAssert.sync(property2(boolean(), boolean(), eq2))
    FcAssert.sync(property3(boolean(), boolean(), boolean(), eq3))
    FcAssert.sync(property4(boolean(), boolean(), boolean(), boolean(), eq4))
    FcAssert.sync(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  })
  it("sync assertProperty", () => {
    assertProperty1(boolean(), eq)
    assertProperty2(boolean(), boolean(), eq2)
    assertProperty3(boolean(), boolean(), boolean(), eq3)
    assertProperty4(boolean(), boolean(), boolean(), boolean(), eq4)
    assertProperty5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5)
  })
  // it("sync beforeEach", () =>
  //   assertParams(property1(boolean(), eq)->beforeEach(_ => ()), testParams1)
  // )
  // it("sync afterEach", () =>
  //   assertParams(property1(boolean(), eq)->afterEach(_ => ()), testParams1)
  // )
})

describe("syncUnit property checks", () => {
  let eq = _ => ()
  let eq2 = (_, _) => ()
  let eq3 = (_, _, _) => ()
  let eq4 = (_, _, _, _) => ()
  let eq5 = (_, _, _, _, _) => ()
  open Property.SyncUnit
  it("syncUnit assert_", () => {
    assert_(property1(boolean(), eq))
    assert_(property2(boolean(), boolean(), eq2))
    assert_(property3(boolean(), boolean(), boolean(), eq3))
    assert_(property4(boolean(), boolean(), boolean(), boolean(), eq4))
    assert_(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  })
  it("syncUnit assert with params", () => {
    assertParams(property1(boolean(), eq), testParams1)
    assertParams(property2(boolean(), boolean(), eq2), Parameters.t(~examples=[(true, true)], ()))
    assertParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )
    assertParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )
    assertParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )
  })
  it("syncUnit check", () => check(property1(boolean(), eq))->validateRunDetails)
  it("syncUnit check with params", () => {
    checkParams(property1(boolean(), eq), testParams1)->validateRunDetails
    checkParams(
      property2(boolean(), boolean(), eq2),
      Parameters.t(~examples=[(true, true)], ()),
    )->validateRunDetails
    checkParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )->validateRunDetails
    checkParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )->validateRunDetails
    checkParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )->validateRunDetails

    pre(true)
  })
  it("syncUnit FcAssert", () => {
    FcAssert.sync(property1(boolean(), eq))
    FcAssert.sync(property2(boolean(), boolean(), eq2))
    FcAssert.sync(property3(boolean(), boolean(), boolean(), eq3))
    FcAssert.sync(property4(boolean(), boolean(), boolean(), boolean(), eq4))
    FcAssert.sync(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  })
  it("syncUnit assertProperty", () => {
    assertProperty1(boolean(), eq)
    assertProperty2(boolean(), boolean(), eq2)
    assertProperty3(boolean(), boolean(), boolean(), eq3)
    assertProperty4(boolean(), boolean(), boolean(), boolean(), eq4)
    assertProperty5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5)
  })
  // it("syncUnit beforeEach", () =>
  //   assertParams(property1(boolean(), eq)->beforeEach(_ => ()), testParams1)
  // )
  // it("syncUnit afterEach", () =>
  //   assertParams(property1(boolean(), eq)->afterEach(_ => ()), testParams1)
  // )
})

open! Promise
describe("async property checks", () => {
  let eq = i => Js.Promise.resolve(i === i)
  let eq2 = (i, j) => Js.Promise.resolve(i === i && j === j)
  let eq3 = (i, j, k) => Js.Promise.resolve(i === i && (j === j && k === k))
  let eq4 = (i, j, k, l) => Js.Promise.resolve(i === i && (j === j && (k === k && l === l)))
  let eq5 = (i, j, k, l, m) =>
    Js.Promise.resolve(i === i && (j === j && (k === k && (l === l && m === m))))
  open Property.Async
  Mocha.Async.it("async assert_ 1", () => assert_(property1(boolean(), eq)))
  Mocha.Async.it("async assert_ 2", () => assert_(property2(boolean(), boolean(), eq2)))
  Mocha.Async.it("async assert_ 3", () => assert_(property3(boolean(), boolean(), boolean(), eq3)))
  Mocha.Async.it("async assert_ 4", () =>
    assert_(property4(boolean(), boolean(), boolean(), boolean(), eq4))
  )
  Mocha.Async.it("async assert_ 5", () =>
    assert_(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  )
  Mocha.Async.it("async assert with params 1", () =>
    assertParams(property1(boolean(), eq), testParams1)
  )
  Mocha.Async.it("async assert with params 2", () =>
    assertParams(property2(boolean(), boolean(), eq2), Parameters.t(~examples=[(true, true)], ()))
  )
  Mocha.Async.it("async assert with params 3", () =>
    assertParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )
  )
  Mocha.Async.it("async assert with params 4", () =>
    assertParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )
  )
  Mocha.Async.it("async assert with params 5", () =>
    assertParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )
  )
  Mocha.Async.it("async check pass", () => {
    pre(true)
    Js.Promise.then_(
      checkResult => {
        validateRunDetails(checkResult)
        Js.Promise.resolve()
      },
      check(property1(boolean(), eq)),
    )
  })
  Mocha.Async.it("async check fail", () =>
    Js.Promise.then_(
      checkResult => {
        validateRunDetails(checkResult)
        Js.Promise.resolve()
      },
      check(property1(boolean(), _ => Js.Promise.resolve(false))),
    )
  )
  Mocha.Async.it("async check with params 1", () =>
    Js.Promise.then_(
      checkResult => {
        validateRunDetails(checkResult)
        Js.Promise.resolve()
      },
      checkParams(property1(boolean(), eq), testParams1),
    )
  )
  Mocha.Async.it("async check with params 2", async () => {
    let checkResult = await checkParams(
      property2(boolean(), boolean(), eq2),
      Parameters.t(~examples=[(true, true)], ()),
    )
    validateRunDetails(checkResult)
  })
  Mocha.Async.it("async check with params 3", async () => {
    let checkResult = await checkParams(
      property3(boolean(), boolean(), boolean(), eq3),
      Parameters.t(~examples=[(true, true, true)], ()),
    )
    validateRunDetails(checkResult)
  })

  Mocha.Async.it("async check with params 4", async () => {
    let checkResult = await checkParams(
      property4(boolean(), boolean(), boolean(), boolean(), eq4),
      Parameters.t(~examples=[(true, true, true, true)], ()),
    )
    validateRunDetails(checkResult)
  })

  Mocha.Async.it("async check with params 5", async () => {
    let checkResult = await checkParams(
      property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5),
      Parameters.t(~examples=[(true, true, true, true, true)], ()),
    )
    validateRunDetails(checkResult)
  })

  Mocha.Async.it("async FcAssert 1", () => FcAssert.async(property1(boolean(), eq)))
  Mocha.Async.it("async FcAssert 2", () => FcAssert.async(property2(boolean(), boolean(), eq2)))
  Mocha.Async.it("async FcAssert 3", () =>
    FcAssert.async(property3(boolean(), boolean(), boolean(), eq3))
  )
  Mocha.Async.it("async FcAssert 4", () =>
    FcAssert.async(property4(boolean(), boolean(), boolean(), boolean(), eq4))
  )
  Mocha.Async.it("async FcAssert 5", () =>
    FcAssert.async(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  )
  Mocha.Async.it("async assertProperty 1", () => assertProperty1(boolean(), eq))
  Mocha.Async.it("async assertProperty 2", () => assertProperty2(boolean(), boolean(), eq2))
  Mocha.Async.it("async assertProperty 3", () =>
    assertProperty3(boolean(), boolean(), boolean(), eq3)
  )
  Mocha.Async.it("async assertProperty 4", () =>
    assertProperty4(boolean(), boolean(), boolean(), boolean(), eq4)
  )
  Mocha.Async.it("async assertProperty 5", () =>
    assertProperty5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5)
  )
  // Mocha.Async.it("async beforeEach", () =>
  //   assertParams(property1(boolean(), eq)->beforeEach(_ => ()), testParams1)
  // )
  // Mocha.Async.it("async afterEach", () =>
  //   assertParams(property1(boolean(), eq)->afterEach(_ => ()), testParams1)
  // )
})

describe("asyncUnit property checks", () => {
  let eq = _ => Js.Promise.resolve()
  let eq2 = (_, _) => Js.Promise.resolve()
  let eq3 = (_, _, _) => Js.Promise.resolve()
  let eq4 = (_, _, _, _) => Js.Promise.resolve()
  let eq5 = (_, _, _, _, _) => Js.Promise.resolve()
  open Property.AsyncUnit
  Mocha.Async.it("asyncUnit assert_ 1", () => assert_(property1(boolean(), eq)))
  Mocha.Async.it("asyncUnit assert_ 2", () => assert_(property2(boolean(), boolean(), eq2)))
  Mocha.Async.it("asyncUnit assert_ 3", () =>
    assert_(property3(boolean(), boolean(), boolean(), eq3))
  )
  Mocha.Async.it("asyncUnit assert_ 4", () =>
    assert_(property4(boolean(), boolean(), boolean(), boolean(), eq4))
  )
  Mocha.Async.it("asyncUnit assert_ 5", () =>
    assert_(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  )
  Mocha.Async.it("asyncUnit assert with params 1", () =>
    assertParams(property1(boolean(), eq), testParams1)
  )
  Mocha.Async.it("asyncUnit assert with params 2", () =>
    assertParams(property2(boolean(), boolean(), eq2), Parameters.t(~examples=[(true, true)], ()))
  )
  Mocha.Async.it("asyncUnit check 2", async () => {
    let checkResult = await check(property2(boolean(), boolean(), eq2))
    validateRunDetails(checkResult)
  })
  Mocha.Async.it("asyncUnit check with params 1", async () => {
    let checkResult = await checkParams(property1(boolean(), eq), testParams1)
    validateRunDetails(checkResult)
  })
  Mocha.Async.it("asyncUnit check with params 2", async () => {
    let checkResult = await checkParams(
      property2(boolean(), boolean(), eq2),
      Parameters.t(~examples=[(true, true)], ()),
    )
    validateRunDetails(checkResult)
  })
  Mocha.Async.it("asyncUnit FcAssert 1", () => FcAssert.async(property1(boolean(), eq)))
  Mocha.Async.it("asyncUnit FcAssert 2", () => FcAssert.async(property2(boolean(), boolean(), eq2)))
  Mocha.Async.it("asyncUnit FcAssert 3", () => FcAssert.async(property3(boolean(), boolean(), boolean(), eq3)))
  Mocha.Async.it("asyncUnit FcAssert 4", () =>
    FcAssert.async(property4(boolean(), boolean(), boolean(), boolean(), eq4))
  )
  Mocha.Async.it("asyncUnit FcAssert 5", () =>
    FcAssert.async(property5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5))
  )
  Mocha.Async.it("asyncUnit assertProperty 1", () => assertProperty1(boolean(), eq))
  Mocha.Async.it("asyncUnit assertProperty 2", () => assertProperty2(boolean(), boolean(), eq2))
  Mocha.Async.it("asyncUnit assertProperty 2", () => assertProperty3(boolean(), boolean(), boolean(), eq3))
  Mocha.Async.it("asyncUnit assertProperty 2", () =>
    assertProperty4(boolean(), boolean(), boolean(), boolean(), eq4)
  )
  Mocha.Async.it("asyncUnit assertProperty 2", () =>
    assertProperty5(boolean(), boolean(), boolean(), boolean(), boolean(), eq5)
  )
  // it("asyncUnit beforeEach", () =>
  //   assertParams(property1(boolean(), eq)->beforeEach(_ => ()), testParams1)
  // )
  // it("asyncUnit afterEach", () =>
  //   assertParams(property1(boolean(), eq)->afterEach(_ => ()), testParams1)
  // )
})
