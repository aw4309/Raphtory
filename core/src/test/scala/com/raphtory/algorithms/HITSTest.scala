package com.raphtory.algorithms

import com.raphtory.BaseCorrectnessTest
import com.raphtory.TestQuery
import com.raphtory.algorithms.generic.HITS

class HITSTest extends BaseCorrectnessTest {
  test("Test HITS on small graph") {
    correctnessTest(
            TestQuery(HITS(truncate = true), 1),
            "HITS/inHITS.csv",
            "HITS/outHITS.csv"
    )
  }

}
