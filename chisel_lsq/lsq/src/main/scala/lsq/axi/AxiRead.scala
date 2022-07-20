//////////////////////////////////////////////////////////////
// IP      : Axi Read Component
// Name    : Omkar Bhilare
// Email-Id: ombhilare999@gmail.com
//////////////////////////////////////////////////////////////

package lsq.axi
import chisel3._
import chisel3.util._
import lsq.config.LsqConfigs

class AxiRead(config: LsqConfigs) extends Module {
  override def desiredName: String = "AXI_READ"

  val io = IO(new Bundle {

    val loadDataFromMem = Output(UInt(config.dataWidth.W))
    val loadAddrToMem = Input(UInt(config.addrWidth.W))
    val loadQIdxForDataOut = Output(UInt(config.dataWidth.W))
    val loadQIdxForDataOutValid = Output(Bool())
    val loadQIdxForAddrIn = Flipped(Decoupled(UInt(config.addrWidth.W)))

    // read address channel
    val ARID     = Output(UInt(config.bufferIdxWidth))
    val ARADDR   = Output(UInt(config.addrWidth.W))
    val ARLEN    = Output(UInt(8.W))
    val ARSIZE   = Output(UInt(3.W))
    val ARBURST  = Output(UInt(2.W))
    val ARLOCK   = Output(Bool())
    val ARCACHE  = Output(UInt(4.W))
    val ARQOS    = Output(UInt(4.W))
    val ARREGION = Output(UInt(4.W))
    val ARPROT   = Output(UInt(3.W))
    val ARVALID  = Output(Bool())
    val ARREADY  = Input(Bool())

    // read data channel
    val RID      = Input(UInt(config.bufferIdxWidth))
    val RDATA    = Input(UInt(config.dataWidth.W))
    val RRESP    = Input(UInt(2.W))
    val RLAST    = Input(Bool())
    val RVALID   = Input(Bool())
    val RREADY   = Output(Bool())

  })

  // Registered Signals:

  //Read Address signals:
  val r_AR_ID    = RegInit(0.U(config.bufferIdxWidth))
  val r_AR_ADDR  = RegInit(0.U(config.addrWidth.W))
  val r_AR_LEN   = RegInit(0.U(8.W))
  val r_AR_SIZE  = RegInit(0.U(3.W))
  val r_AR_BURST = RegInit(0.U(2.W))
  val r_AR_QOS    = RegInit(0.U(4.W))
  val r_AR_LOCK   = RegInit(0.U(1.W))
  val r_AR_CACHE  = RegInit(0.U(4.W))
  val r_AR_REGION = RegInit(0.U(4.W))
  val r_AR_PROT  = RegInit(0.U(3.W))
  val r_AR_VALID = RegInit(0.U(1.W))
  val r_AR_READY = RegInit(0.U(1.W))
  
  //Read Data/Response Channel
  val r_R_RDATA   = RegInit(0.U(32.W))
  val r_R_READY  = RegInit(0.U(1.W))

  //Extra Variables read:
  val rx_transaction_cnt    = RegInit(0.U(3.W))
  val rx_len                = RegInit(0.U(8.W))
  val read_response_ready  = Wire(UInt(1.W))

  //Initializing Variables:
  read_response_ready  := 0.U
  read_response_ready  := io.RVALID & ~io.RRESP & io.RLAST

  // Other Signals:
  val idxArray = RegInit(VecInit(Seq.fill(config.bufferDepth)(0.U(config.fifoIdxWidth))))
  val waitingForData = RegInit(VecInit(Seq.fill(config.bufferDepth)(false.B)))
  val firstFreeIdx = PriorityEncoder(VecInit(waitingForData map (x => !x)))
  val hasFreeIdx = waitingForData.exists((x : Bool) => !x)

  // Object for Write State Machine 
  //object Rx_State extends ChiselEnum {
  //    val sIdle, sOne, sTwo, sThree = Value
  //}

  //val rx_state = RegInit(Rx_State.sIdle)

  val sIdle :: sOne :: sTwo :: sThree = Enum(3)
  val rx_state = RegInit(sIdle)

    //Write State Machine
    switch(rx_state) {   
        is(sIdle){
            when(io.loadQIdxForAddrIn.valid === 1.U) {               //IF write is asserted by the top module
                when(rx_transaction_cnt === 0.U) {
                    //First Transaction:
                    r_AR_ADDR  := io.loadAddrToMem
                    r_AR_LEN   := 0.U
                    r_AR_SIZE  := log2Ceil(config.dataWidth).U
                    r_AR_BURST := 1.U
                    rx_len     := 1.U         //(io.TOP_R_LENGTH << io.TOP_R_SIZE) + 1.U 
                    r_AR_LOCK  := 0.U
                    r_AR_CACHE := 0.U
                    r_AR_PROT  := 0.U
                    r_AR_QOS   := 0.U
                    r_AR_REGION := 0.U
                    r_AR_VALID := hasFreeIdx && io.loadQIdxForAddrIn.valid  //1.U
                    r_AR_ID    := firstFreeIdx

                    r_R_READY  := 1.U

                    when(io.ARREADY === 1.U) {     //Valid and Ready High at
                        rx_transaction_cnt := 0.U 
                        rx_state := sOne
                        r_AR_VALID := 0.U
                    } .otherwise {
                        rx_transaction_cnt := rx_transaction_cnt + 1.U
                    }

                } .otherwise {
                    when(io.ARREADY === 1.U){
                        rx_transaction_cnt := 0.U
                        r_AR_VALID         := 0.U
                        rx_state := sOne
                    } .otherwise {
                        //Hold the values
                        r_AR_ADDR  := io.loadAddrToMem
                        r_AR_BURST := 1.U
                        r_AR_LEN   := 0.U
                        r_AR_SIZE  := log2Ceil(config.dataWidth).U
                        r_AR_VALID := 1.U
                        r_R_READY  := 1.U
                        r_AR_ID    := 0.U
                        r_AR_PROT  := 0.U                           
                        rx_transaction_cnt := rx_transaction_cnt + 1.U
                    }
                }
            } .otherwise {
                rx_state := sIdle
            }
        }
        is(sOne){
            when(io.RVALID === 1.U){
                when (rx_len >= 1.U){
                    rx_len    := rx_len - 1.U 
                    r_R_RDATA := io.RDATA
                    rx_state  := sOne
                    when (read_response_ready === 1.U){
                        r_R_READY := 0.U
                        rx_state := sIdle
                    }
                }
            } .otherwise {
                r_R_RDATA := 0.U
                rx_state  := sOne
            }
        }
    }

  //io.ARVALID := hasFreeIdx && io.loadQIdxForAddrIn.valid
  io.loadQIdxForAddrIn.ready := hasFreeIdx && io.ARREADY
  io.loadQIdxForDataOut := idxArray(io.RID)
  io.loadQIdxForDataOutValid := (io.RVALID && io.RRESP === 0.U)
  //io.loadDataFromMem := io.RDATA  

  // Stalling Loop:
  for(i <- 0 until config.bufferDepth){
    when(i.U === firstFreeIdx && hasFreeIdx && io.ARREADY && io.loadQIdxForAddrIn.valid) {
      idxArray(i) := io.loadQIdxForAddrIn.bits
      waitingForData(i) := true.B
    }.elsewhen( i.U === io.RID && io.RVALID && io.RRESP === 0.U){
      waitingForData(i) := false.B
    }
  }

    //Updating AXI Signals:
    //Read Address Channel:
    io.ARBURST := r_AR_BURST
    io.ARADDR  := r_AR_ADDR
    io.ARLEN   := r_AR_LEN
    io.ARSIZE  := r_AR_SIZE
    io.ARID    := r_AR_ID
    io.ARVALID := r_AR_VALID.asBool() 
    io.ARPROT  := r_AR_PROT
    io.ARQOS    := r_AR_QOS
    io.ARREGION := r_AR_REGION
    io.ARLOCK   := r_AR_LOCK.asBool() 
    io.ARCACHE  := r_AR_CACHE  
    
    //Read Data/Response Channel:
    io.RREADY   := r_R_READY.asBool() 

    //Signals to Top Module:
    io.loadDataFromMem := r_R_RDATA

}