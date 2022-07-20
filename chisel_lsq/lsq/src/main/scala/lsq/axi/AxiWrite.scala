//////////////////////////////////////////////////////////////
// IP      : Axi Write Component
// Name    : Omkar Bhilare
// Email-Id: ombhilare999@gmail.com
//////////////////////////////////////////////////////////////

package lsq.axi
import chisel3._
import chisel3.util._
import lsq.config.LsqConfigs
//import chisel3.experimental.ChiselEnum

class AxiWrite(config: LsqConfigs) extends Module {
  override def desiredName: String = "AXI_WRITE"

//Defining IOs of the module

    val io= IO(
        new Bundle{
            //Signals from the Top:
            val storeAddrToMem = Input(UInt(config.addrWidth.W))
            val storeDataToMem = Input(UInt(config.dataWidth.W))
            val storeQIdxInToAW = Flipped(Decoupled(UInt(config.fifoIdxWidth)))
            val storeQIdxOutFromAW = Output(UInt(config.fifoIdxWidth))
            val storeQIdxOutFromAWValid = Output(Bool())

            //AXI write address channel:
            val AWID = Output(UInt(config.bufferIdxWidth))
            val AWADDR = Output(UInt(config.addrWidth.W))
            val AWLEN = Output(UInt(8.W))
            val AWSIZE = Output(UInt(3.W))
            val AWBURST = Output(UInt(2.W))
            val AWPROT = Output(UInt(3.W))
            val AWVALID = Output(Bool())
            val AWREADY = Input(Bool())
            val AWLOCK = Output(Bool())
            val AWCACHE = Output(UInt(4.W))
            val AWQOS = Output(UInt(4.W))
            val AWREGION = Output(UInt(4.W))

            //AXI read address channel:
            val WID = Output(UInt(config.bufferIdxWidth))
            val WDATA = Output(UInt(config.dataWidth.W))
            val WSTRB = Output(UInt((config.dataWidth / 8).W))
            val WLAST = Output(Bool())
            val WVALID = Output(Bool())
            val WREADY = Input(Bool())    

            //Write Response Channel:
            val BID = Input(UInt(config.bufferIdxWidth))
            val BRESP = Input(UInt(2.W))
            val BVALID = Input(Bool())
            val BREADY = Output(Bool())    
        }
    )
        // Other Signals:
        val idxArray = RegInit(VecInit(Seq.fill(config.bufferDepth)(0.U(config.fifoIdxWidth))))
        val waitingForResponse = RegInit(VecInit(Seq.fill(config.bufferDepth)(false.B)))
        val firstFreeIdx = PriorityEncoder(VecInit(waitingForResponse map (x => !x)))
        val hasFreeIdx = waitingForResponse.exists((x: Bool) => !x)

        // Signals for the other modules:
        io.storeQIdxOutFromAWValid := io.BVALID && (io.BRESP === 0.U)
        io.storeQIdxOutFromAW      := idxArray(io.BID)
        io.storeQIdxInToAW.ready := false.B

        // Registered Signals
        
        //Write Address Signals:
        val r_AW_BURST = RegInit(0.U(2.W))
        val r_AW_ADDR  = RegInit(0.U(6.W))
        val r_AW_LEN   = RegInit(0.U(8.W))
        val r_AW_SIZE  = RegInit(0.U(3.W))
        val r_AW_ID    = RegInit(0.U(1.W))
        val r_AW_VALID = RegInit(0.U(1.W))
        val r_AW_PROT  = RegInit(0.U(3.W))
        val r_AW_QOS    = RegInit(0.U(4.W))
        val r_AW_LOCK   = RegInit(0.U(1.W))
        val r_AW_CACHE  = RegInit(0.U(4.W))
        val r_AW_REGION = RegInit(0.U(4.W))

        //Write Data Signals:
        val r_W_DATA   = RegInit(0.U(32.W))
        val r_W_LAST   = RegInit(0.U(1.W))
        val r_W_STRB   = RegInit(0.U(4.W))
        val r_W_VALID  = RegInit(0.U(1.W))
        val r_W_ID     = RegInit(0.U(1.W))

        //Write Response:
        val r_B_READY  = RegInit(0.U(1.W))

        //Extra Variables write:
        val r_transaction_cnt     = RegInit(0.U(3.W))
        val r_len                 = RegInit(0.U(8.W))
        val write_response_ready  = Wire(UInt(1.W))
        
        //Initializing Variables:
        write_response_ready := 0.U  
        write_response_ready := io.BVALID & ~io.BRESP

        // Object for Write State Machine 
        //object State extends ChiselEnum {
        //    val sIdle, sOne, sTwo, sThree = Value
        //}

        //val state = RegInit(State.sIdle)
        val sIdle :: sOne :: sTwo :: sThree  = Enum(3)
        val state = RegInit(sIdle)


        //Write State Machine
        switch(state) {   
            is(sIdle) {
                when(io.storeQIdxInToAW.valid === 1.U) {//IF write is asserted by the top module
                    when (r_transaction_cnt === 0.U){   //Means the first step of transaction
                        //First Transaction:
                        r_B_READY  := 0.U                //Last Transaction reamining task

                        r_AW_LEN   := 0.U
                        r_AW_SIZE  := log2Ceil(config.dataWidth).U
                        r_AW_BURST := 1.U
                        r_AW_LOCK   := 0.U
                        r_AW_CACHE  := 0.U
                        r_AW_PROT  := 0.U
                        r_AW_QOS    := 0.U
                        r_AW_REGION := 0.U
                        r_AW_ID    := firstFreeIdx
                        r_AW_ADDR  := io.storeAddrToMem
                        r_len      := 1.U               //r_len      := ( io.TOP_LENGTH << io.TOP_SIZE ) + 1.U
                        r_AW_VALID := 1.U
                        
                        //Write Related Signals:
                        r_W_DATA   := io.storeDataToMem
                        r_W_STRB   := "hf".asUInt(4.W)
                        r_W_VALID  := 1.U 

                        // Some other signals:
                        io.storeQIdxInToAW.ready := true.B
                        
                        when (io.AWREADY === 1.U){ //Valid and Ready high at the same time
                            r_transaction_cnt := 0.U
                            state := sOne
                            r_AW_VALID := 0.U
                        } .otherwise {
                            r_transaction_cnt := r_transaction_cnt + 1.U  //Increment on each transaction 
                        }
                    } .otherwise {
                        when (io.AWREADY === 1.U){ //Valid and Ready high at the same time
                            r_transaction_cnt := 0.U
                            r_AW_VALID := 0.U
                            state := sOne
                        } .otherwise {
                            // Hold the Values
                            r_AW_BURST := r_AW_BURST
                            r_AW_ADDR  := r_AW_ADDR
                            r_AW_LEN   := r_AW_LEN
                            r_AW_SIZE  := r_AW_SIZE
                            r_W_DATA   := r_W_DATA
                            r_AW_VALID := 1.U
                            r_AW_ID    := 0.U
                            r_AW_PROT  := 0.U                           
                            r_transaction_cnt := r_transaction_cnt + 1.U  //Increment on each transaction 
                        }
                    } 
                } .otherwise {
                    state := sIdle
                }
            } 
            is(sOne){
                when(io.WREADY === 1.U) {  
                    when (r_len > 0.U) { 
                        r_len      := r_len - 1.U
                        r_W_DATA   := io.storeDataToMem
                        r_W_ID     := firstFreeIdx
                        state      := sOne     
                        r_transaction_cnt := r_transaction_cnt + 1.U   
                        when (r_len === 1.U){
                            r_W_LAST   := 1.U      
                        }
                    } .otherwise {
                        r_transaction_cnt := 0.U
                        state := sTwo      //Go to next state 
                        r_W_LAST  := 0.U
                        r_W_DATA   := 0.U    
                        r_W_STRB   := 0.U
                        r_W_VALID  := 0.U
                    }
                } .otherwise {
                    when (r_transaction_cnt === 0.U){ //Valid and Ready high at the same time
                        r_W_DATA   := io.storeDataToMem
                    } .otherwise {
                        r_W_DATA   := r_W_DATA
                    }
                    r_W_LAST   := r_W_LAST               
                    r_W_STRB   := r_W_STRB
                    r_W_VALID  := r_W_VALID
                    state := sOne
                }
            }
            is(sTwo){
                r_B_READY := 1.U        //Stating master is ready to accept the write response
                when (write_response_ready === 1.U) {
                    state     := sIdle
                }
            }
        }

        // Stalling Loop:
        for (i <- 0 until config.bufferDepth) {
          when(i.U === firstFreeIdx && io.storeQIdxInToAW.ready) {
            waitingForResponse(i) := true.B
            idxArray(firstFreeIdx) := io.storeQIdxInToAW.bits
          }.elsewhen(i.U === io.BID) {
            waitingForResponse(i) := false.B
          }
        }

        //Updating AXI Signals:

        //Write Address Signals:
        io.AWBURST := r_AW_BURST 
        io.AWADDR  := r_AW_ADDR  
        io.AWLEN   := r_AW_LEN  
        io.AWSIZE  := r_AW_SIZE 
        io.AWID    := r_AW_ID    
        io.AWVALID := r_AW_VALID.asBool() 
        io.AWPROT  := r_AW_PROT 
        io.AWQOS   := r_AW_QOS
        io.AWREGION := r_AW_REGION
        io.AWLOCK  := r_AW_LOCK.asBool() 
        io.AWCACHE := r_AW_CACHE 

        //Write Data Signals:
        io.WDATA   := r_W_DATA
        io.WLAST   := r_W_LAST.asBool() 
        io.WSTRB   := r_W_STRB
        io.WVALID  := r_W_VALID.asBool() 
        io.WID     := r_W_ID

        //Write Response Channel:
        io.BREADY  := r_B_READY.asBool() 
}