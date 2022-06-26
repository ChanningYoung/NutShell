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

import chisel3._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.subsystem.RocketCrossingParams
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

case class NutTileParams(
  core: NutCoreParams = NutCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  btb: Option[BTBParams] = Some(BTBParams()),
  // dataScratchpadBytes: Int = 0,
  name: Option[String] = Some("tile"),
  hartId: Int = 0,
  beuAddr: Option[BigInt] = None,
  blockerCtrlAddr: Option[BigInt] = None,
  boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
  ) extends TileParams {
  require(icache.isDefined)
  require(dcache.isDefined)
}

class NutTile private(
     val nutParams: NutTileParams,
     crossing: ClockCrossingType,
     lookup: LookupByHartIdImpl,
     q: Parameters,
     logicalTreeNode: LogicalTreeNode)
  extends BaseTile(nutParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
  with HasHellaCache
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(
    params: NutTileParams, crossing: RocketCrossingParams, lookup: LookupByHartIdImpl, logicalTreeNode: LogicalTreeNode
  )(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p, logicalTreeNode)

  override lazy val module = new NutTileModuleImp(this)

  val masterNode: TLEphemeralNode = visibilityNode
  val slaveNode: TLIdentityNode = TLIdentityNode()
  val intOutwardNode: IntIdentityNode = IntIdentityNode()

  val nutLogicalTree = new NutLogicalTreeNode(this, p(XLen))

  val dtim_adapter: Option[ScratchpadSlavePort] = tileParams.dcache.flatMap { d => d.scratch.map { s =>
    val coreParams = {
      class C(implicit val p: Parameters) extends HasCoreParameters
      new C
    }
    LazyModule(
      new ScratchpadSlavePort(
        AddressSet.misaligned(s, d.dataScratchpadBytes), coreParams.coreDataBytes,
        tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO
      )
    )
  }}
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, lm.node.portParams.head.beatBytes))

  val bus_error_unit: Option[BusErrorUnit[L1BusErrors]] = nutParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a), nutLogicalTree))
    intOutwardNode := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  val tile_master_blocker: Option[BasicBusBlocker] =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  nDCachePorts += 1 /* core */ + dtim_adapter.isDefined.toInt

  val dtimProperty = dtim_adapter.map(d => Map("sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("oscpu,nut0", "riscv")) {
    override def parent: Some[Device] = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty ++ tileProperties)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(hartId))
  }

  override def makeMasterBoundaryBuffers(implicit p: Parameters): TLNode = {
    if (!nutParams.boundaryBuffers) { super.makeMasterBoundaryBuffers }
    else { TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1)) }
  }

  override def makeSlaveBoundaryBuffers(implicit  p: Parameters): TLNode = {
    if (!nutParams.boundaryBuffers) { super.makeSlaveBoundaryBuffers }
    else { TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none) }
  }

  val dCacheLogicalTreeNode = new DCacheLogicalTreeNode(dcache, dtim_adapter.map(_.device), nutParams.dcache.get)
  LogicalModuleTree.add(nutLogicalTree, dCacheLogicalTreeNode)
}

class NutTileModuleImp(outer: NutTile) extends BaseTileModuleImp(outer)
    with HasHellaCacheModule
