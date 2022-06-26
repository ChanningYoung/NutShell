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
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.{TLB, _}
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import oscpu.nutshell.nutcore._
import oscpu.nutshell.utils._

class NutFrontendIO(implicit p: Parameters) extends CoreBundle()(p) {
  val out: Vec[DecoupledIO[DecodeIO]] = Vec(2, Decoupled(new DecodeIO))
  val flushVec: UInt = Output(UInt(4.W))
  val redirect: RedirectIO = Flipped(new RedirectIO)
}

// NutCore frontend with Rocket ICache
class NutFrontend(val icacheParams: ICacheParams, hartid: Int)(implicit p: Parameters, q: NutCoreConfig)
    extends LazyModule {
  lazy val module = new NutFrontendModule(this)
  val icache: freechips.rocketchip.rocket.ICache = LazyModule(new ICache(icacheParams, hartid))
  val masterNode: TLClientNode = icache.masterNode
  val slaveNode: TLManagerNode = icache.slaveNode
}

class NutFrontendBundle(val outer: NutFrontend) extends CoreBundle()(outer.p)
    with HasExternallyDrivenTileConstants {
  val cpu: NutFrontendIO = new NutFrontendIO().flip()
  val ptw = new TLBPTWIO()
  val errors = new ICacheErrors
}

@chiselName
class NutFrontendModule(outer: NutFrontend)(implicit q: NutCoreConfig) extends LazyModuleImp(outer)
    with HasNutCoreParameters
    with HasL1ICacheParameters {
  val io: NutFrontendBundle = IO(new NutFrontendBundle(outer))
  implicit val edge: TLEdgeOut = outer.masterNode.edges.out.head
  val icache: ICacheModule = outer.icache.module
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val tlb: TLB = Module(new freechips.rocketchip.rocket.TLB(
    true, log2Ceil(fetchBytes), freechips.rocketchip.rocket.TLBConfig(nTLBEntries)
  ))

  val ifu: IFU_inorder          = Module(new IFU_inorder)
  val ibf: NaiveRVCAlignBuffer  = Module(new NaiveRVCAlignBuffer)
  val idu: IDU                  = Module(new IDU)

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
                                  isFlush: Bool, entries: Int = 4, pipe: Boolean = false): Unit = {
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  pipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineConnect(ibf.io.out, idu.io.in(0), idu.io.out(0).fire(), ifu.io.flushVec(1))
  idu.io.in(1) := DontCare

  ibf.io.flush := ifu.io.flushVec(1)
  io.cpu.out <> idu.io.out
  io.cpu.redirect <> ifu.io.redirect
  io.cpu.flushVec <> ifu.io.flushVec
}