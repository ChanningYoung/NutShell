/**************************************************************************************
  * Copyright (c) 2020 Institute of Computing Technology, CAS
  * Copyright (c) 2020 University of Chinese Academy of Sciences
  *
  * NutShell is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *             http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
  * FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package oscpu.nutshell.tile

import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.util.BooleanToAugmentedBoolean
import freechips.rocketchip.rocket.BTBParams

case class OMNutBranchPredictor(
 nBtbEntries: Int,
 nBhtEntries: Int,
 nRasEntries: Int,
 _types: Seq[String] = Seq("OMNutBranchPredictor", "OMBranchPredictor", "OMComponent", "OMCompoundType")
) extends OMBranchPredictor

case class OMNutCore(
  isa: OMISA,
  mulDiv: Option[OMMulDiv],
  fpu: Option[OMFPU],
  performanceMonitor: Option[OMPerformanceMonitor],
  pmp: Option[OMPMP],
  documentationName: String,
  hartIds: Seq[Int],
  hasVectoredInterrupts: Boolean,
  interruptLatency: Int,
  nLocalInterrupts: Int,
  nBreakpoints: Int,
  branchPredictor: Option[OMNutBranchPredictor],
  dcache: Option[OMDCache],
  icache: Option[OMICache],
  busErrorUnit: Option[OMBusError],
  hasClockGate: Boolean,
  hasSCIE: Boolean,
  _types: Seq[String] = Seq("OMNutCore", "OMCore", "OMComponent", "OMCompoundType")
) extends OMCore

object NutOMBTB {
  def makeOMI(p: BTBParams): OMNutBranchPredictor = {
    OMNutBranchPredictor(
      nBtbEntries = p.nEntries,
      nBhtEntries = p.bhtParams.map(_.nEntries).getOrElse(0),
      nRasEntries = p.nRAS
    )
  }
}

object NutOMISA {
  def customExtensions(coreParams: NutCoreParams): List[OMCustomExtensionSpecification] = {
    if (coreParams.haveCFlush) List (Xsifivecflushdlone()) else Nil
  }

  def nutISA(coreParams: NutCoreParams, xLen: Int): OMISA = {
    val baseInstructionSet = xLen match {
      case 32 => if (coreParams.useRVE) RV32E else RV32I
      case 64 => if (coreParams.useRVE) RV64E else RV64I
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
    }

    val isaExtSpec = ISAExtensions.specVersion _

    val baseSpec = BaseExtensions.specVersion _

    val baseISAVersion = baseInstructionSet match {
      case RV32E => "1.9"
      case RV32I => "2.0"
      case RV64E => "1.9"
      case RV64I => "2.0"
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid baseISAVersion: $baseInstructionSet")
    }

    val addressTranslationModes = xLen match {
      case 32 => Sv32
      case 64 => Sv39
      case _ => throw new IllegalArgumentException(s"ERROR: Invalid Xlen: $xLen")
    }

    OMISA(
      xLen = xLen,
      baseSpecification = baseSpec(baseInstructionSet, baseISAVersion),
      base = baseInstructionSet,
      m = coreParams.mulDiv.map(x => isaExtSpec(M, "2.0")),
      a = coreParams.useAtomics.option(isaExtSpec(A, "2.0")),
      f = coreParams.fpu.map(x => isaExtSpec(F, "2.0")),
      d = coreParams.fpu.filter(_.fLen > 32).map(x => isaExtSpec(D, "2.0")),
      c = coreParams.useCompressed.option(isaExtSpec(C, " 2.0")),
      u = (coreParams.useVM || coreParams.useUser).option(isaExtSpec(U, "1.10")),
      s = coreParams.useVM.option(isaExtSpec(S, "1.10")),
      addressTranslationModes = Seq(addressTranslationModes),
      customExtensions = customExtensions(coreParams)
    )
  }
}

object NutPerformanceMonitor {
  def perfmon(coreParams: NutCoreParams): Option[OMPerformanceMonitor] = {
    if (coreParams.haveBasicCounters || coreParams.nPerfCounters > 0) {
      Some(OMPerformanceMonitor(
        specifications = List[OMSpecification](PrivilegedArchitectureExtensions.specVersion(MachineLevelISA, "1.10")),
        hasBasicCounters = coreParams.haveBasicCounters,
        nAdditionalCounters = coreParams.nPerfCounters
      ))
    } else {
      None
    }
  }
}

object NutOMPMP {
  def pmp(coreParams: NutCoreParams): Option[OMPMP] = {
    if (coreParams.pmpGranularity > 0 || coreParams.nPMPs > 0) {
      Some(OMPMP(
        specifications = List[OMSpecification](PrivilegedArchitectureExtensions.specVersion(MachineLevelISA, "1.10")),
        nRegions = coreParams.nPMPs,
        granularity = coreParams.pmpGranularity
      ))
    }
    else {
      None
    }
  }
}