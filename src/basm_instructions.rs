






#[derive(Debug, Clone, Copy)]
pub enum BasmInstruction{
    NOP,
    PUSH,
    DROP,
    DUP,
    SWAP,
    PLUSI,
    MINUSI,
    MULTI,
    DIVI,
    MODI,
    MULTU,
    DIVU,
    MODU,
    PLUSF,
    MINUSF,
    MULTF,
    DIVF,
    JMP,
    JMPIf,
    RET,
    CALL,
    NATIVE,
    HALT,
    NOT,
    EQI,
    GEI,
    GTI,
    LEI,
    LTI,
    NEI,
    EQU,
    GEU,
    GTU,
    LEU,
    LTU,
    NEU,
    EQF,
    GEF,
    GTF,
    LEF,
    LTF,
    NEF,
    ANDB,
    ORB,
    XOR,
    SHR,
    SHL,
    NOTB,
    READ8U,
    READ16U,
    READ32U,
    READ64U,
    READ8I,
    READ16I,
    READ32I,
    READ64I,
    WRITE8,
    WRITE16,
    WRITE32,
    WRITE64,
    I2F,
    U2F,
    F2I,
    F2U,
}


pub fn basm_instruction_opcode(instruction: &BasmInstruction) -> i64 {
    match instruction {
        BasmInstruction::NOP =>     0,
        BasmInstruction::PUSH =>    1,
        BasmInstruction::DROP =>    2,
        BasmInstruction::DUP =>     3,
        BasmInstruction::SWAP =>    4,
        BasmInstruction::PLUSI =>   5,
        BasmInstruction::MINUSI =>  6,
        BasmInstruction::MULTI =>   7,
        BasmInstruction::DIVI =>    8,
        BasmInstruction::MODI =>    9,
        BasmInstruction::MULTU =>   10,
        BasmInstruction::DIVU =>    11,
        BasmInstruction::MODU =>    12,
        BasmInstruction::PLUSF =>   13,
        BasmInstruction::MINUSF =>  14,
        BasmInstruction::MULTF =>   15,
        BasmInstruction::DIVF =>    16,
        BasmInstruction::JMP =>     17,
        BasmInstruction::JMPIf =>   18,
        BasmInstruction::RET =>     19,
        BasmInstruction::CALL =>    20,
        BasmInstruction::NATIVE =>  21,
        BasmInstruction::HALT =>    22,
        BasmInstruction::NOT =>     23,
        BasmInstruction::EQI =>     24,
        BasmInstruction::GEI =>     25,
        BasmInstruction::GTI =>     26,
        BasmInstruction::LEI =>     27,
        BasmInstruction::LTI =>     28,
        BasmInstruction::NEI =>     29,
        BasmInstruction::EQU =>     30,
        BasmInstruction::GEU =>     31,
        BasmInstruction::GTU =>     32,
        BasmInstruction::LEU =>     33,
        BasmInstruction::LTU =>     34,
        BasmInstruction::NEU =>     35,
        BasmInstruction::EQF =>     36,
        BasmInstruction::GEF =>     37,
        BasmInstruction::GTF =>     38,
        BasmInstruction::LEF =>     39,
        BasmInstruction::LTF =>     40,
        BasmInstruction::NEF =>     41,
        BasmInstruction::ANDB =>    42,
        BasmInstruction::ORB =>     43,
        BasmInstruction::XOR =>     44,
        BasmInstruction::SHR =>     45,
        BasmInstruction::SHL =>     46,
        BasmInstruction::NOTB =>    47,
        BasmInstruction::READ8U =>  48,
        BasmInstruction::READ16U => 49,
        BasmInstruction::READ32U => 50,
        BasmInstruction::READ64U => 51,
        BasmInstruction::READ8I =>  52,
        BasmInstruction::READ16I => 53,
        BasmInstruction::READ32I => 54,
        BasmInstruction::READ64I => 55,
        BasmInstruction::WRITE8 =>  56,
        BasmInstruction::WRITE16 => 57,
        BasmInstruction::WRITE32 => 58,
        BasmInstruction::WRITE64 => 59,
        BasmInstruction::I2F =>     60,
        BasmInstruction::U2F =>     61,
        BasmInstruction::F2I =>     62,
        BasmInstruction::F2U =>     63,
    }
}