module Language.ZMachine.Opcode
    (

    ) where

import RIO

import Data.Binary.Get
import Data.Bits
import Language.ZMachine.ZSCII (ZsciiString)

-- All Opcodes are converted to Word16 whether short or long
type Opcode = Word16
data OpcodeKind = OP0 | OP1 | OP2 | VAR | EXT
data VarRef = Stack | LocalVar Word8 | GlobalVar Word8
data Operand = OperandShort Word8 | OperandLong Word16 | OperandVar VarRef | OperandMissing
type BranchOffset = Word16


data ZInstr = ZInstr
    { opcode :: !Opcode
    , operandKind :: !OpcodeKind
    , operands :: ![Operand]
    }


-- Extra things that may come after an instruction
-- , storeVar :: !(Maybe VarRef)
-- , branch :: !BranchOffset
-- , text :: !(Maybe ZsciiString)



--  Decode enough of the instruction to deduce the opcode and kind
decodeInstruction :: Get ZInstr
decodeInstruction =
    do byte1 <- lookAhead getWord8
       case byte1 of
            b | b < 0x20 -> mkInstr OP2 longOp [smallc, smallc]
            b | b < 0x40 -> mkInstr OP2 longOp [smallc, varRef]
            b | b < 0x60 -> mkInstr OP2 longOp [varRef, smallc]
            b | b < 0x80 -> mkInstr OP2 longOp [varRef, varRef]
            b | b < 0x90 -> mkInstr OP1 shortOp [longc] 
            b | b < 0xa0 -> mkInstr OP1 shortOp [smallc]
            b | b < 0xb0 -> mkInstr OP1 shortOp [varRef]
            -- TODO : Only V5+
            0xbe         -> mkInstr EXT extOp []
            b | b < 0xc0 -> mkInstr OP0 shortOp []
            b | b < 0xe0 -> do
                opcode <- shortOp -- types in next byte
                return (ZInstr opcode OP2 [])
            _            -> do
                opcode <- shortOp -- types in next byte(s)
                return (ZInstr opcode VAR [])

    where
        longOp = do opcode <- getWord16be
                    return (opcode .&. 0x1f)
        shortOp = do opcode <- getWord8
                     return (fromIntegral (opcode .&. 0xf))
        extOp = do _ <- getWord8
                   fromIntegral <$> getWord8

        mkInstr kind opDecoder opaDecoders = do
            opcode <- opDecoder
            operands <- sequence opaDecoders
            return (ZInstr opcode kind operands)

        smallc = OperandShort <$> getWord8

        longc = OperandLong <$> getWord16be

        varRef = do
            byte <- getWord8 
            return $ case byte of
                        0x00 -> OperandVar Stack
                        x | x < 0x10 -> OperandVar (LocalVar x)
                        x -> OperandVar (GlobalVar x)



