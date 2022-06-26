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

import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._

import oscpu.nutshell.nutcore.HasNutCoreParameter

case class NutCoreParams(
  bootFreqHz: BigInt = 0,
  useVM: Boolean = true,
  useUser: Boolean = false,
  useDebug: Boolean = true,
  useAtomics: Boolean = true,
  useAtomicsOnlyForIO: Boolean = false,
  useCompressed: Boolean = true,
  useSCIE: Boolean = false,
  useRVE: Boolean = false,
  mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams()),
  fpu: Option[FPUParams] = Some(FPUParams()),
  nLocalInterrupts: Int = 0,
  nPMPs: Int = 8,
  nBreakpoints: Int = 1,
  useBPWatch: Boolean = false,
  nPerfCounters: Int = 0,
  haveBasicCounters: Boolean = true,
  misaWritable: Boolean = true,
  haveCFlush: Boolean = false,
  nL2TLBEntries: Int = 0,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  // fastLoadWord: Boolean = true,
  // fastLoadByte: Boolean = false,
  // branchPredictionModeCSR: Boolean = false,
  clockGate: Boolean = false,
  // mvendorid: Int = 0, // 0 means non-commercial implementation
  // mimpid: Int = 0x20181004, // release date in BCD
  ) extends freechips.rocketchip.tile.CoreParams
{
  val fetchWidth: Int = if (useCompressed) 4 else 2
  val decodeWidth: Int = 1
  val retireWidth: Int = 1
  val instBits: Int = if (useCompressed) 16 else 32
  val pmpGranularity: Int = 4
  val haveFSDirty = false
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
}

trait HasNutCoreParameters extends HasCoreParameters with HasNutCoreParameter {
  lazy val nutParams: NutCoreParams = tileParams.core.asInstanceOf[NutCoreParams]
}