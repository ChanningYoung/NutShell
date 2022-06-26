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

import freechips.rocketchip.diplomacy.ResourceBindings
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model._

class NutLogicalTreeNode(
  tile: NutTile,
  XLen: Int
) extends LogicalTreeNode(() => Some(tile.cpuDevice)) {
  override def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    val nutParams = tile.nutParams
    val coreParams = nutParams.core

    // Expect that one of the components passed in is the DCache/DTIM.
    val omDCache = components.collectFirst { case x: OMDCache => x }.get

    // Expect that one of the components passed in is the ICache.
    val omICache = components.collectFirst { case x: OMICache => x }.get

    val omBusError = components.collectFirst { case x: OMBusError => x }

    Seq(OMNutCore(
      isa = NutOMISA.nutISA(coreParams, XLen),
      mulDiv = coreParams.mulDiv.map { md => OMMulDiv.makeOMI(md, XLen) },
      fpu = coreParams.fpu.map { f => OMFPU(fLen = f.fLen) },
      performanceMonitor = NutPerformanceMonitor.perfmon(coreParams),
      pmp = NutOMPMP.pmp(coreParams),
      documentationName = nutParams.name.getOrElse("nut"),
      hartIds = Seq(nutParams.hartId),
      hasVectoredInterrupts = true,
      interruptLatency = 4,
      nLocalInterrupts = coreParams.nLocalInterrupts,
      nBreakpoints = coreParams.nBreakpoints,
      branchPredictor = nutParams.btb.map(NutOMBTB.makeOMI),
      dcache = Some(omDCache),
      icache = Some(omICache),
      busErrorUnit = omBusError,
      hasClockGate = coreParams.clockGate,
      hasSCIE = coreParams.useSCIE
    ))
  }
}
